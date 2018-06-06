#include "wsp.h"

struct wsp_file* wsp_open_file(const char *filename) {
    // Open the file
    int wsp_fd = open(filename, O_RDWR);
    if (wsp_fd < 0) {
        perror("Failed to open whisper file");
        return NULL;
    }

    // MMAP it
    off_t wsp_file_size = lseek(wsp_fd, 0, SEEK_END);
    void *wsp_map = mmap(
        NULL, // System may choose address
        wsp_file_size, // Number of bytes to be initialized from FD
        PROT_READ | PROT_WRITE,  // Need to read/write the file
        MAP_SHARED, // All processes can see changes, changes persisted
        wsp_fd, // WSP file fd
        0 // No flags
    );
    if (wsp_map == (void *) -1) {
        perror("Failed to mmap whisper file");
        close(wsp_fd);
        return NULL;
    }

    // Start parsing
    struct wsp_file *result = malloc(sizeof(struct wsp_file));
    result->wsp_data = wsp_map;
    result->wsp_fd = wsp_fd;
    result->wsp_size = wsp_file_size;
    parse_header(wsp_map, &result->header);

    // Alloc archive header info
    result->archives = malloc(
        sizeof(struct wsp_archive*) * result->header.archive_count);
    for (uint32_t i = 0; i < result->header.archive_count; i++) {
        result->archives[i] = malloc(sizeof(struct wsp_archive));
        parse_archive_header(
            wsp_map,
            WSP_HEADER_BYTES + WSP_ARCHIVE_HEADER_BYTES * i,
            result->archives[i]
        );
    }

    return result;
}

void wsp_close(struct wsp_file *wsp) {
    // Unmap the file
    munmap(wsp->wsp_data, wsp->wsp_size);

    // Close the underlying fd
    close(wsp->wsp_fd);

    // Manually free the archive info
    for (uint32_t i = 0; i < wsp->header.archive_count; i++) {
        free(wsp->archives[i]);
    }

    // Free the archive container array
    free(wsp->archives);
}

int wsp_get_point(struct wsp_file *wsp, uint32_t archive, uint32_t point_offset,
                   uint32_t *timestamp, double *value) {
    // Sanity check the archive index
    if (archive > wsp->header.archive_count) {
        fprintf(stderr, "Archive #%d > archive count of %d\n",
            archive,
            wsp->header.archive_count
        );
        return -1;
    }

    // Sanity check the point offset
    if (point_offset > wsp->archives[archive]->points) {
        fprintf(stderr, "Archive #%d has no point at index %d (size: %d)\n",
            archive,
            point_offset,
            wsp->archives[archive]->points
        );
        return -1;
    }

    // Get the archive offset
    const uint32_t archive_offset = wsp->archives[archive]->offset;

    // Calculate the data offset
    const uint32_t data_offset = archive_offset + (point_offset * WSP_POINT_BYTES);

    // Nab the point timestamp
    *timestamp = COMPOSE_U32(wsp->wsp_data, data_offset);

    // Parse the value
    uint64_t value_bytes = COMPOSE_U64(wsp->wsp_data, data_offset + 4);
    *value = *(double *) &value_bytes;

    return 0;
}

void parse_header(uint8_t *wsp_data, struct wsp_header *dest) {
    dest->aggregation_type = COMPOSE_U32(wsp_data, 0);
    dest->max_retention = COMPOSE_U32(wsp_data, 4);
    uint32_t xff_32 = COMPOSE_U32(wsp_data, 8);
    dest->xff = *(float *) &xff_32;
    dest->archive_count = COMPOSE_U32(wsp_data, 12);
}

void write_header(uint8_t *wsp_data, struct wsp_header *src) {
    WRITE_U32(wsp_data,  0, src->aggregation_type);
    WRITE_U32(wsp_data,  4, src->max_retention);
    uint32_t xff_32 = *(uint32_t *) &src->xff;
    WRITE_U32(wsp_data,  8, xff_32);
    WRITE_U32(wsp_data, 12, src->archive_count);
}

void parse_archive_header(uint8_t *wsp_data, size_t offset, struct wsp_archive *dest) {
    dest->offset = COMPOSE_U32(wsp_data, offset);
    dest->seconds_per_point = COMPOSE_U32(wsp_data, offset + 4);
    dest->points = COMPOSE_U32(wsp_data, offset + 8);
}

void write_archive_header(uint8_t *wsp_data, size_t offset, struct wsp_archive *src) {
    WRITE_U32(wsp_data, offset, src->offset);
    WRITE_U32(wsp_data, offset + 4, src->seconds_per_point);
    WRITE_U32(wsp_data, offset + 8, src->points);
}

static ERL_NIF_TERM erl_wsp_open(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
){
    // Assert we got one argument
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    // Ensure that the arg is a binary
    if (!enif_is_binary(env, argv[0])) {
        return mk_error(env, "not_a_binary");
    }

    // Copy the binary metadata into the structs
    ErlNifBinary filename_bin;
    enif_inspect_binary(env, argv[0], &filename_bin);

    // Ensure that the filename has a null terminator
    char safe_filename[filename_bin.size + 1];
    memcpy(safe_filename, filename_bin.data, filename_bin.size);
    safe_filename[filename_bin.size] = '\0';

    // Try and load that file as a whisper db
    struct wsp_file *wsp = wsp_open_file(safe_filename);

    // If it's null, gotta bail
    if (!wsp) {
        return mk_error(env, "wsp_open_failed");
    }

    // Allocate a managed pointer to this file struct
    struct priv_data *priv_data = enif_priv_data(env);
    struct wsp_file** erl_wsp_ptr = enif_alloc_resource(
        priv_data->wsp_file_resource,
        sizeof(struct wsp_file*)
    );

    // Stash the pointer
    *erl_wsp_ptr = wsp;

    // Create an opaque resource
    ERL_NIF_TERM erl_wsp_resource = enif_make_resource(env, erl_wsp_ptr);

    // Relinquish the ref we own
    enif_release_resource(erl_wsp_ptr);

    // Return {ok, Wsp}
    return enif_make_tuple2(env, mk_atom(env, "ok"), erl_wsp_resource);
}

static ERL_NIF_TERM erl_wsp_get_storage_schema(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
) {
    // Assert we got one argument
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    // Extract the stashed wsp_file pointer
    struct priv_data *priv_data = enif_priv_data(env);
    struct wsp_file** erl_wsp_ptr;
    if (!enif_get_resource(env, argv[0],
                           priv_data->wsp_file_resource,
                           ((void*) (&erl_wsp_ptr)))) {
        return mk_error(env, "bad_internal_state");
    }
    struct wsp_file* wsp = *erl_wsp_ptr;

    // Iterate over the archive headers, and accumulate a tuple for each
    ERL_NIF_TERM result = enif_make_list(env, 0);
    for (uint32_t i = 0; i < wsp->header.archive_count; i++) {
        ERL_NIF_TERM duration = enif_make_uint(
            env, wsp->archives[i]->seconds_per_point * wsp->archives[i]->points
        );
        ERL_NIF_TERM bucketing = enif_make_uint(
            env, wsp->archives[i]->seconds_per_point
        );
        ERL_NIF_TERM schema = enif_make_tuple2(env, duration, bucketing);
        result = enif_make_list_cell(env, schema, result);
    }

    return enif_make_tuple2(env, mk_atom(env, "ok"), result);
}

static ERL_NIF_TERM erl_wsp_fetch(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
) {
    // We expect 3-4 args:
    // - Our stashed wsp file
    // - The current time in seconds
    // - Timestamp to get data since
    // - Optional timestamp to bound data until
    if(argc != 3 && argc != 4) {
        return enif_make_badarg(env);
    }

    // Extract the stashed wsp_file pointer
    struct priv_data *priv_data = enif_priv_data(env);
    struct wsp_file** erl_wsp_ptr;
    if (!enif_get_resource(env, argv[0],
                           priv_data->wsp_file_resource,
                           ((void*) (&erl_wsp_ptr)))) {
        return mk_error(env, "bad_internal_state");
    }
    struct wsp_file* wsp = *erl_wsp_ptr;

    // Get the current timestamp
    unsigned now_seconds;
    if (!enif_get_uint(env, argv[1], &now_seconds)) {
        return mk_error(env, "bad_now_timestamp");
    }

    // Get the from timestamp
    unsigned from_seconds;
    if (!enif_get_uint(env, argv[2], &from_seconds)) {
        return mk_error(env, "bad_from_timestamp");
    }

    // Get the until timestamp, if present
    unsigned until_seconds;
    if (argc == 4) {
        if (!enif_get_uint(env, argv[3], &until_seconds)) {
            return mk_error(env, "bad_until_timestamp");
        }
    } else {
        until_seconds = now_seconds;
    }

    // Sanity check
    if (from_seconds > until_seconds) {
        return mk_error(env, "from_before_until");
    }

    // If from is in the future, we have no data, return []
    if (from_seconds > now_seconds) {
        return enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_list(env, 0));
    }

    // If the until is before our max retention, we have no data
    const uint32_t oldest_time = now_seconds - wsp->header.max_retention;
    if (until_seconds < oldest_time) {
        return enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_list(env, 0));
    }

    // Bound the from/until timestamps to be within the retention period / now
    from_seconds = oldest_time > from_seconds ? oldest_time : from_seconds;
    until_seconds = until_seconds > now_seconds ? now_seconds : until_seconds;

    // The retention time necessary for an archive to have our data
    const uint32_t diff = now_seconds - from_seconds;

    // Find the most precise archive with retention >= diff
    struct wsp_archive *best_archive = NULL;
    for (uint32_t i = 0; i < wsp->header.archive_count; i++) {
        const uint32_t archive_retention = (
            wsp->archives[i]->seconds_per_point * wsp->archives[i]->points
        );
        if (best_archive == NULL ||
                (archive_retention >= diff
                 && wsp->archives[i]->seconds_per_point < best_archive->seconds_per_point)) {
            best_archive = wsp->archives[i];
        }
    }

    // If we can't figure out what archive to use, that's a problem
    if (best_archive == NULL) {
        return mk_error(env, "no_applicable_archives");
    }

    // Fetch the data from the archive
    return erl_wsp_fetch_archive(
        env, wsp, best_archive, from_seconds, until_seconds);
}

static ERL_NIF_TERM erl_wsp_fetch_archive(
    ErlNifEnv *env,
    struct wsp_file *wsp, struct wsp_archive *archive,
    const uint32_t from_seconds, const uint32_t until_seconds) {

    // Get the start/end intervals
    const uint32_t step = archive->seconds_per_point;
    const uint32_t from_interval = (from_seconds - (from_seconds % step)) + step;
    uint32_t until_interval = (until_seconds - (until_seconds % step)) + step;

    // If the intervals are the same, capture at least one point
    if (from_interval == until_interval) {
        until_interval += step;
    }

    // Get the offset of the first datum in the archive
    const uint32_t data_offset = archive->offset;
    const uint32_t base_interval = COMPOSE_U32(wsp->wsp_data, data_offset);

    // If the base interval is 0, this file has no data - return []
    if (base_interval == 0) {
        return enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_list(env, 0));
    }

    // Calculate the begin/end byte offsets
    // N.B. that the end offset might be before the begin offset, if the data
    // has begun to wrap.
    const uint32_t total_point_bytes = archive->points * WSP_POINT_BYTES;

    const int32_t start_delta = from_interval - base_interval; // can be neg
    const int32_t start_bucket_offset = start_delta / step;
    const uint32_t start_byte_offset = data_offset + (
        start_bucket_offset > 0
        ? ((start_bucket_offset % archive->points) * WSP_POINT_BYTES)
        : total_point_bytes - ((start_bucket_offset % archive->points) * WSP_POINT_BYTES)
    );

    const int32_t end_delta = from_interval - base_interval; // can be neg
    const int32_t end_bucket_offset = end_delta / step;
    const uint32_t end_byte_offset = data_offset + (
        end_bucket_offset > 0
        ? ((end_bucket_offset % archive->points) * WSP_POINT_BYTES)
        : total_point_bytes - ((end_bucket_offset % archive->points) * WSP_POINT_BYTES)
    );

    // Create our result list
    ERL_NIF_TERM result_arr = enif_make_list(env, 0);

    // Starting at the end offset, read backwards, warping if needed, and load
    // the data into the result list.
    uint32_t datum_byte_offset = end_byte_offset;
    do {
        // Grab the time for this datapoint
        uint32_t datum_time = COMPOSE_U32(wsp->wsp_data, datum_byte_offset);

        // If the time is within range, add this point
        if (datum_time >= from_interval && datum_time <= until_interval) {
            // Extract the value as well
            uint64_t value_bytes = COMPOSE_U64(wsp->wsp_data, datum_byte_offset + 4);
            double datum_value = *(double *) &value_bytes;

            // Create the erlang term for this point and cons it to the list
            // Note that we convert NaNs to the atom 'nan', since erlang
            // doesn't like actual NaN floats
            ERL_NIF_TERM erl_datum_value = (datum_value != datum_value ?
                    mk_atom(env, "nan") : enif_make_double(env, datum_value));
            ERL_NIF_TERM erl_datum_time = enif_make_uint(env, datum_time);
            ERL_NIF_TERM point = enif_make_tuple2(env, erl_datum_value, erl_datum_time);
            result_arr = enif_make_list_cell(env, point, result_arr);
        }

        // Move backwards to the next datapoint
        datum_byte_offset -= WSP_POINT_BYTES;
        if (datum_byte_offset < data_offset) {
            // Wrap back around to the end of the archive
            datum_byte_offset = data_offset + (archive->points - 1) * WSP_POINT_BYTES;
        }
    } while (datum_byte_offset != start_byte_offset);

    return enif_make_tuple2(env, mk_atom(env, "ok"), result_arr);
}

static ERL_NIF_TERM erl_wsp_update(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
) {
    // We expect 4 args:
    // - Our stashed wsp file
    // - The current time in seconds
    // - The value to store
    // - The value timestamp
    if(argc != 4) {
        return enif_make_badarg(env);
    }

    // Extract the stashed wsp_file pointer
    struct priv_data *priv_data = enif_priv_data(env);
    struct wsp_file** erl_wsp_ptr;
    if (!enif_get_resource(env, argv[0],
                           priv_data->wsp_file_resource,
                           ((void*) (&erl_wsp_ptr)))) {
        return mk_error(env, "bad_internal_state");
    }
    struct wsp_file* wsp = *erl_wsp_ptr;

    // Get the current timestamp
    unsigned now_seconds;
    if (!enif_get_uint(env, argv[1], &now_seconds)) {
        return mk_error(env, "bad_now_timestamp");
    }

    // Get the value
    double value;
    if (!enif_get_double(env, argv[2], &value)) {
        return mk_error(env, "bad_value");
    }

    // Get the value timestamp
    unsigned value_timestamp;
    if (!enif_get_uint(env, argv[3], &value_timestamp)) {
        return mk_error(env, "bad_value_timestamp");
    }

    // Check the diff on the ts is within archive range
    const unsigned diff = now_seconds - value_timestamp;
    if (!(diff >= 0 && diff < wsp->header.max_retention)) {
        return mk_error(env, "diff_outside_archive_range");
    }

    // Get the highest precision archive that can fit this datum
    struct wsp_archive *best_archive = NULL;
    for (uint32_t i = 0; i < wsp->header.archive_count; i++) {
        const uint32_t archive_retention = (
            wsp->archives[i]->seconds_per_point * wsp->archives[i]->points
        );
        if (best_archive == NULL ||
                (diff < archive_retention
                 && wsp->archives[i]->seconds_per_point < best_archive->seconds_per_point)) {
            best_archive = wsp->archives[i];
        }
    }

    // If we didn't get a good archive, something is up
    if (best_archive == NULL) {
        return mk_error(env, "no_suitable_archives");
    }

    // Read the first point in the archive to get an offset
    const size_t data_offset = best_archive->offset;
    uint32_t base_interval = COMPOSE_U32(wsp->wsp_data, data_offset);

    const uint32_t interval = value_timestamp - (
        value_timestamp % best_archive->seconds_per_point
    );

    if (base_interval == 0) {
        // Brand new file, write to the first index
        WRITE_U32(wsp->wsp_data, data_offset, interval);
        uint64_t val_u64 = *(uint64_t *) &value;
        WRITE_U64(wsp->wsp_data, data_offset+4, val_u64);
    } else {
        // Figure out the offset in time between the first datum and the new
        const uint32_t time_offset = interval - base_interval;

        // Convert that to an offset in points based on the bucket size
        const uint32_t point_offset = time_offset / best_archive->seconds_per_point;

        // Figure how many bytes to offset by
        const uint32_t byte_offset = point_offset * WSP_POINT_BYTES;

        // Get the actual address to update, using the archive offset + wrapping
        // if we need to
        const uint32_t archive_size_bytes = best_archive->points * WSP_POINT_BYTES;
        const uint32_t update_offset = data_offset + (byte_offset % archive_size_bytes);

        // At long last, update that offset.
        WRITE_U32(wsp->wsp_data, update_offset, interval);
        uint64_t val_u64 = *(uint64_t *) &value;
        WRITE_U64(wsp->wsp_data, update_offset+4, val_u64);
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM erl_wsp_create(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
) {
    // Assert we got two args
    if(argc != 2) {
        return enif_make_badarg(env);
    }

    // Ensure that the first arg is a binary
    if (!enif_is_binary(env, argv[0])) {
        return mk_error(env, "not_a_binary");
    }

    // Copy the binary metadata into the structs
    ErlNifBinary filename_bin;
    enif_inspect_binary(env, argv[0], &filename_bin);

    // Ensure that the filename has a null terminator
    char safe_filename[filename_bin.size + 1];
    memcpy(safe_filename, filename_bin.data, filename_bin.size);
    safe_filename[filename_bin.size] = '\0';

    // Ensure that the second arg is a list
    if (!enif_is_list(env, argv[1])) {
        return mk_error(env, "not_a_list");
    }

    // Get the number of storage schemas
    unsigned storage_schema_count;
    int success = enif_get_list_length(env, argv[1], &storage_schema_count);
    if (!success) {
        return mk_error(env, "not_a_list");
    }

    // Validate the schemas & count up the total needed file size
    size_t wsp_file_size = WSP_HEADER_BYTES;
    uint32_t wsp_max_retention = 0;
    ERL_NIF_TERM schema_traverse = argv[1];
    for (unsigned i = 0; i < storage_schema_count; i++) {
        // Each schema needs a header
        wsp_file_size += WSP_ARCHIVE_HEADER_BYTES;

        // Get this list element
        ERL_NIF_TERM current_schema;
        if (!enif_get_list_cell(env, schema_traverse, &current_schema, &schema_traverse)) {
            return mk_error(env, "list_traversal_error");
        }

        // Check the schema tuple is legit
        const ERL_NIF_TERM *arr;
        int arity;
        if (!enif_get_tuple(env, current_schema, &arity, &arr)
            || arity != 2) {
            return mk_error(env, "bad_schema_tuple");
        }

        // Get the two uints out
        unsigned duration;
        unsigned seconds_per_bucket;
        if (!enif_get_uint(env, arr[0], &duration)) {
            return mk_error(env, "bad_duration");
        }
        if (!enif_get_uint(env, arr[1], &seconds_per_bucket)) {
            return mk_error(env, "bad_bucketing");
        }

        // Update the max duration val
        if (duration > wsp_max_retention) {
            wsp_max_retention = duration;
        }

        // Number of points = duration / seconds per bucket
        // Add an extra point if it's not an even division
        const unsigned point_count = (
            (duration / seconds_per_bucket) +
            (duration % seconds_per_bucket > 0 ? 1 : 0)
        );

        // Increment the file size, since that's why we're even here
        wsp_file_size += point_count * WSP_POINT_BYTES;
    }

    // Now that we know how big to make it, let's start making the wsp file
    int wsp_fd = open(safe_filename, O_RDWR | O_CREAT, WSP_PERMS);
    if (wsp_fd < 0) {
        perror("Failed to open file");
        return mk_error(env, "fd_open_failed");
    }

    // Now truncate it to the size we need
    if (ftruncate(wsp_fd, wsp_file_size) == -1) {
        perror("Failed to set filesize");
        close(wsp_fd);
        return mk_error(env, "ftruncate_failed");
    }

    // MMAP it
    void *wsp_map = mmap(
        NULL, // System may choose address
        wsp_file_size, // Number of bytes to be initialized from FD
        PROT_READ | PROT_WRITE,  // Need to read/write the file
        MAP_SHARED, // All processes can see changes, changes persisted
        wsp_fd, // WSP file fd
        0 // No flags
    );
    if (wsp_map == (void *) -1) {
        perror("Failed to mmap whisper file");
        close(wsp_fd);
        return mk_error(env, "mmap_failed");
    }

    // Store the wsp file info
    struct wsp_file *wsp = malloc(sizeof(struct wsp_file));
    wsp->wsp_data = wsp_map;
    wsp->wsp_fd = wsp_fd;
    wsp->wsp_size = wsp_file_size;

    // Set & write the file header
    wsp->header.aggregation_type = WSP_AGG_AVERAGE;
    wsp->header.max_retention = wsp_max_retention;
    wsp->header.xff = 0.5;
    wsp->header.archive_count = storage_schema_count;
    write_header(wsp_map, &wsp->header);

    // Prep the archive header array
    wsp->archives = malloc(
        sizeof(struct wsp_archive*) * storage_schema_count);

    // Now we need to iterate the archives again
    // Skip the checks this time, since we know they must be valid
    schema_traverse = argv[1];
    uint32_t previous_point_size = 0;
    for (unsigned i = 0; i < storage_schema_count; i++) {
        // Alloc the header struct
        wsp->archives[i] = malloc(sizeof(struct wsp_archive));

        // Offset is header size
        //  + archive header size * archive count
        //  + previous archive points size
        wsp->archives[i]->offset = (
            WSP_HEADER_BYTES +
            WSP_ARCHIVE_HEADER_BYTES * storage_schema_count +
            previous_point_size
        );

        // Get this list element
        ERL_NIF_TERM current_schema;
        enif_get_list_cell(env, schema_traverse, &current_schema, &schema_traverse);

        // Get the tuple
        const ERL_NIF_TERM *arr;
        int arity;
        enif_get_tuple(env, current_schema, &arity, &arr);

        // Get the two uints
        unsigned duration;
        unsigned seconds_per_bucket;
        enif_get_uint(env, arr[0], &duration);
        enif_get_uint(env, arr[1], &seconds_per_bucket);

        // Number of points = duration / seconds per bucket
        // Add an extra point if it's not an even division
        const unsigned point_count = (
            (duration / seconds_per_bucket) +
            (duration % seconds_per_bucket > 0 ? 1 : 0)
        );

        // Set the point data
        wsp->archives[i]->seconds_per_point = seconds_per_bucket;
        wsp->archives[i]->points = point_count;

        // Increment the point size offset
        previous_point_size += (point_count * WSP_POINT_BYTES);

        // Update the mmap'd file with the data
        const size_t header_offset = (
            WSP_HEADER_BYTES + WSP_ARCHIVE_HEADER_BYTES * i
        );
        write_archive_header(wsp_map, header_offset, wsp->archives[i]);
    }

    // Allocate a managed pointer to this file struct
    struct priv_data *priv_data = enif_priv_data(env);
    struct wsp_file** erl_wsp_ptr = enif_alloc_resource(
        priv_data->wsp_file_resource,
        sizeof(struct wsp_file*)
    );

    // Stash the pointer
    *erl_wsp_ptr = wsp;

    // Create an opaque resource
    ERL_NIF_TERM erl_wsp_resource = enif_make_resource(env, erl_wsp_ptr);

    // Relinquish the ref we own
    enif_release_resource(erl_wsp_ptr);

    // Return {ok, Wsp}
    return enif_make_tuple2(env, mk_atom(env, "ok"), erl_wsp_resource);
}

// Module callbacks
int load(ErlNifEnv* env, void** priv_data, UNUSED ERL_NIF_TERM load_info) {
    // We need to create a new resource type for the computation state
    // we pass around.
    ErlNifResourceFlags tried;
    ErlNifResourceType *wsp_file_type = enif_open_resource_type(
        env,
        NULL, // modue_str (unused, must be NULL)
        "wsp_file",
        erl_wsp_file_destructor, // No destructor
        ERL_NIF_RT_CREATE,
        &tried
    );

    // Check that the resource type was created correctly, and if not exit
    // with a non-zero status
    if (!wsp_file_type) {
        return 1;
    }

    // Store the resource type in our private data
    struct priv_data *data = malloc(sizeof(struct priv_data));
    data->wsp_file_resource = wsp_file_type;

    // Update the priv_data with our priv_data struct
    *priv_data = data;

    // Return success
    return 0;
}

int upgrade(UNUSED ErlNifEnv* env, UNUSED void** priv_data,
            UNUSED void** old_priv_data, UNUSED ERL_NIF_TERM load_info) {
    // Nothing needs to be done when the module is reloaded
    return 0;
}

void unload(UNUSED ErlNifEnv* env, void* priv_data){
    // We need to free priv_data, which is a pointer to our PrivData struct
    free(priv_data);
}

void erl_wsp_file_destructor(ErlNifEnv* env, void* obj) {
    struct wsp_file **erl_wsp_ptr = obj;
    wsp_close(*erl_wsp_ptr);
}

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;
    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }
    return ret;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg) {
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}


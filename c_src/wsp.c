#include "wsp.h"

struct wsp_file* wsp_open_file(const char *filename) {
    // Open the file
    DLOG("Opening file %s\n", filename);
    int wsp_fd = open(filename, O_RDWR);
    if (wsp_fd < 0) {
        perror("Failed to open whisper file");
        return NULL;
    }

    // MMAP it
    off_t wsp_file_size = lseek(wsp_fd, 0, SEEK_END);
    DLOG("MMAPing file %s\n", filename);
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
    DLOG("Agg type: %d, Max retention: %d, Xff: %f, Archive count: %d\n",
        result->header.aggregation_type,
        result->header.max_retention,
        result->header.xff,
        result->header.archive_count
    );

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
        DLOG("Archive %d: Offset %d, Secs/point: %d, Points: %d\n",
            i,
            result->archives[i]->offset,
            result->archives[i]->seconds_per_point,
            result->archives[i]->points
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

void parse_archive_header(uint8_t *wsp_data, size_t offset, struct wsp_archive *dest) {
    dest->offset = COMPOSE_U32(wsp_data, offset);
    dest->seconds_per_point = COMPOSE_U32(wsp_data, offset + 4);
    dest->points = COMPOSE_U32(wsp_data, offset + 8);
}

void main(int argc, char **argv) {
    struct wsp_file *wsp = wsp_open_file(argv[1]);
    uint32_t timestamp;
    double value;
    for (uint32_t i = 0; i < wsp->header.archive_count; i++) {
        fprintf(stderr, "Archive %d\n", i);
        for (uint32_t p = 0; p < wsp->archives[i]->points; p++) {
            wsp_get_point(wsp, i, p, &timestamp, &value);
            fprintf(stderr, "[%d] %f\n", timestamp, value);
        }
    }
}

static ERL_NIF_TERM erl_wsp_open(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
){
    // Assert we got one argument
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    // Ensure that the arg is a binary
    if(enif_is_binary(env, argv[0])) {
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

    // For now just close it again lol
    return mk_atom(env, "ok");
}

static ERL_NIF_TERM erl_wsp_create(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
) {

}

// Module callbacks
int load(ErlNifEnv* env, void** priv_data, UNUSED ERL_NIF_TERM load_info) {
    // We need to create a new resource type for the computation state
    // we pass around.
    ErlNifResourceFlags tried;
    ErlNifResourceType *levenshtein_state_type = enif_open_resource_type(
        env,
        NULL, // modue_str (unused, must be NULL)
        "levenshtein_state",
        NULL, // No destructor
        ERL_NIF_RT_CREATE,
        &tried
    );

    // Check that the resource type was created correctly, and if not exit
    // with a non-zero status
    if (!levenshtein_state_type) {
        return 1;
    }

    // Store the resource type in our private data
    struct priv_data *data = malloc(sizeof(struct priv_data));

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


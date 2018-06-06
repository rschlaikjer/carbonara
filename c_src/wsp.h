#ifndef LIBWSP_H
#define LIBWSP_H

#define _POSIX_C_SOURCE 199309L

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "erl_nif.h"

#define WSP_PERMS (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)

#define WSP_HEADER_BYTES 16
#define WSP_ARCHIVE_HEADER_BYTES 12
#define WSP_POINT_BYTES 12

#define COMPOSE_U32(data, offset) ( \
    (data[offset] << 24) | (data[offset+1] << 16) | \
    (data[offset+2] << 8) | data[offset+3])

#define COMPOSE_U64(data, offset) ( \
    (((uint64_t) data[offset]) << 56) | (((uint64_t) data[offset+1]) << 48) | \
    (((uint64_t) data[offset+2] << 40)) | (((uint64_t) data[offset+3] << 32)) | \
    (data[offset+4] << 24) | (data[offset+5] << 16) | \
    (data[offset+6] << 8) | data[offset+7])

#define WRITE_U32(data, offset, value) \
    data[offset] = (value >> 24) & 0xFF; \
    data[offset+1] = (value >> 16) & 0xFF; \
    data[offset+2] = (value >>  8) & 0xFF; \
    data[offset+3] = (value >>  0) & 0xFF

// Whisper aggregation strategies
#define WSP_AGG_AVERAGE 1
#define WSP_AGG_SUM 2
#define WSP_AGG_LAST 3
#define WSP_AGG_MAX 4
#define WSP_AGG_MIN 5
#define WSP_AGG_AVERAGE_ZERO 6
#define WSP_AGG_ABSMIN 7
#define WSP_AGG_ABSMAX 8

#define DEBUG

#ifdef DEBUG
#define DLOG(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
#else
#define DLOG(fmt, ...)
#endif

// For hushing compiler warnings
#define UNUSED __attribute__((unused))

struct wsp_header {
    uint32_t aggregation_type;
    uint32_t max_retention;
    float xff;
    uint32_t archive_count;
};

struct wsp_archive {
    uint32_t offset;
    uint32_t seconds_per_point;
    uint32_t points;
};

struct wsp_file {
    struct wsp_header header;
    struct wsp_archive **archives;
    int wsp_fd;
    off_t wsp_size;
    uint8_t *wsp_data;
};

struct priv_data {
    ErlNifResourceType *wsp_file_resource;
};

struct wsp_file* wsp_open_file(const char *filename);

void parse_header(uint8_t *wsp_data, struct wsp_header *dest);

void parse_archive_header(uint8_t *wsp_data, size_t offset, struct wsp_archive *dest);

// Exported methods
static ERL_NIF_TERM erl_wsp_open(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);
static ERL_NIF_TERM erl_wsp_create(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);
static ERL_NIF_TERM erl_wsp_get_storage_schema(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);

// Internal term manipulation
ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);

// Destructor for Erlang wsp wrapper
void erl_wsp_file_destructor(ErlNifEnv* env, void* obj);

// Module callbacks
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
void unload(ErlNifEnv* env, void* priv_data);

// Export methods
static ErlNifFunc nif_funcs[] = {
    {"open", 1, erl_wsp_open, 0},
    {"create", 2, erl_wsp_create, 0},
    {"get_storage_schema", 1, erl_wsp_get_storage_schema, 0}
};
ERL_NIF_INIT(wsp, nif_funcs, load, NULL, upgrade, unload);

#endif // LIBWSP_H

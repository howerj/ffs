/* Forth File System */
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

#define MAX_PATH     (256u)
#define BLKSZ        (1024u)
#define FAT_BLK_FREE (0u)
#define FAT_BLK_BAD  (65531u)
#define FAT_BLK_RES  (65532u)
#define FAT_BLK_STP  (65533u)
#define FAT_BLK_END  (65534u)
#define VERSION      (0x666u)
#define MIN(X, Y)    ((X) < (Y) ? (X) : (Y))

typedef struct {
	uint8_t buf[BLKSZ];
	uint8_t number, dirty, error;
	FILE *storage;
} block_t;

typedef struct {
	uint16_t version;
	uint16_t number, count;
	uint16_t fat_start, fat_blocks, fat_usable_blocks;
	uint16_t data_start, data_blocks;

	uint16_t free, bad, other; /* FAT statistics */
		
	unsigned mounted :1, error :1;
	char file[32];
	char volname[32];
	char path[MAX_PATH];
} ffs_t;

static int flush(block_t *b) {
	assert(b);
	if (b->error)
		goto fail;
	if (b->storage == NULL)
		goto fail;
	if (b->number == 0) {
		b->dirty = 0;
		return 0;
	}
	if (b->dirty) {
		if (fseek(b->storage, b->number * BLKSZ, SEEK_SET) < 0)
			goto fail;
		if (BLKSZ != fwrite(b->buf, 1, BLKSZ, b->storage))
			goto fail;
		b->dirty = 0;
	}
	return 0;
fail:
	b->error = 1;
	b->dirty = 0;
	b->number = 0;
	return 0;
}

static int update(block_t *b) {
	assert(b);
	if (b->number)
		b->dirty = 1;
	return 0;
}

static int block(block_t *b, uint16_t number) {
	assert(b);
	if (b->error)
		goto fail;
	if (b->storage == NULL)
		goto fail;
	if (number == 0)
		goto fail;
	if (b->number == number)
		return 0;
	if (flush(b) < 0)
		goto fail;
	if (fseek(b->storage, number * BLKSZ, SEEK_SET) < 0)
		goto fail;
	if (BLKSZ != fread(b->buf, 1, BLKSZ, b->storage))
		goto fail;
	b->number = number;
	return 0;
fail:
	b->error = 1;
	b->dirty = 0;
	b->number = 0;
	return -1;
}

static void w16(uint8_t *m, uint16_t v) {
	assert(m);
	m[0] = v;
	m[1] = v >> 8;
}

static uint16_t r16(uint8_t *m) {
	assert(m);
	return (uint16_t)m[0] | ((uint16_t)m[1] << 8);
}

/*static int fat_set_block(ffs_t *t, block_t *b) {
	assert(t);
	assert(b);
	return 0;
}*/

static uint16_t fat_find_free_block(ffs_t *t, block_t *b) { /* returns 0 on non-found */
	assert(t);
	assert(b);
	for (size_t i = 0; i < t->fat_usable_blocks; i++) {
		const uint16_t quo = t->fat_start + i / (2u * BLKSZ);
		const uint16_t rem = i % (2u * BLKSZ);
		if (block(b, quo) < 0) /* error will be picked up later */
			return 0;
		const uint16_t blk = r16(&b->buf[rem * 2u]);
		if (blk == FAT_BLK_FREE) {
			/*if (exchange) {
				w16(&b->buf[rem * 2u], value);
				update(b);
			}*/
			return blk;
		}
		if (blk == FAT_BLK_END)
			return 0;
	}
	return 0;
}

static int ffs_format(ffs_t *t, block_t *b) {
	assert(b);
	assert(t);
	if (t->number == 0)
		return -1;
	FILE *f = fopen(t->file, "wb+");
	if (!f)
		return -1;
	memset(b->buf, 0, BLKSZ);
	for (size_t i = 0; i < (t->number + t->count); i++) {
		if (BLKSZ != fwrite(b->buf, 1, BLKSZ, f))
			goto fail;
	}
	b->storage = f;
	if (block(b, t->number) < 0)
		return -1;
	t->fat_blocks  = (t->count / (2u * BLKSZ)) + 1u;
	t->fat_start   = t->number + 1u;
	t->fat_usable_blocks = (t->count - (t->fat_blocks + 1u + 1u + 1u));
	t->data_start  = t->fat_start + t->fat_blocks + 1u;
	t->data_blocks = t->count - t->data_start;
	w16(&b->buf[8], t->version);
	w16(&b->buf[10], t->number);
	w16(&b->buf[12], t->count);
	w16(&b->buf[14], t->fat_blocks);
	w16(&b->buf[16],  t->data_start);
	w16(&b->buf[18], t->data_blocks);
	memcpy(&b->buf[32], t->file, MIN(32, sizeof(t->file)));
	memcpy(&b->buf[64], t->volname, MIN(32, sizeof(t->volname)));
	update(b);
	for (size_t i = t->fat_start; i < (t->fat_start + t->fat_blocks); i++) {
		block(b, i);
		for (size_t j = 0; j < (BLKSZ / 2u); j += 2)
			w16(&b->buf[j], FAT_BLK_END);
		update(b);
	}
	for (size_t i = 0; i < t->fat_usable_blocks; i++) {
		uint16_t quo = t->fat_start + i / (2u * BLKSZ);
		uint16_t rem = i % (2u * BLKSZ);
		block(b, quo);
		w16(&b->buf[rem * 2], 0);
	}
	if (flush(b) < 0)
		goto fail;
	if (fclose(f) < 0)
		goto fail;
	return 0;
fail:
	if (f)
		fclose(f);
	return -1;
}

static int ffs_mount(ffs_t *f, block_t *b) {
	assert(f);
	assert(b);
	

	return 0;
}

static int ffs_unmount(ffs_t *f, block_t *b) {
	assert(f);
	assert(b);
	return 0;
}

static int ffs_stat(ffs_t *f, block_t *b) {
	assert(f);
	assert(b);
	if (f->error)
		goto fail;
	if (ffs_mount(f, b) < 0)
		goto fail;
	return 0;
fail:
	f->error = 1;
	return -1;

}

/* MKDIR, LS, RM, CAT, OPEN/CLOSE/SEEK/READ/WRITE, STAT, TOUCH, CD, */

int main(int argc, char **argv) {
	if (argc < 2) {
		(void)fprintf(stderr, "usage: %s cmd...\n", argv[0]);
		return 1;
	}
	block_t b = { .dirty = 0, };
	ffs_t t = { .version = VERSION, .number = 1, .count = 32, .file = "ffs.fb", .volname = "FORTH BLOCK FILE SYSTEM", };
	if (!strcmp(argv[1], "format"))
		return ffs_format(&t, &b) < 0;
	if (!strcmp(argv[1], "stat")) {
		if (ffs_stat(&t, &b) < 0)
			return 1;
		return 0;
	}
	return 0;
}


import math 
from PIL import Image, ImageDraw  
import sys, drgn
from drgn import NULL, Object, cast, container_of, execscript, reinterpret, sizeof
from drgn.helpers.linux import *
from drgn.helpers.linux.mm import for_each_page, pfn_to_page, _vmemmap
from drgn.helpers.linux.fs import inode_paths


PGLocked = 1 << prog.constant('PG_locked')
PGReferenced = 1 << prog.constant('PG_referenced')
PGUptodate = 1 << prog.constant('PG_uptodate')
PGDirty = 1 << prog.constant('PG_dirty')
PGLru = 1 << prog.constant('PG_lru')
PGActive = 1 << prog.constant('PG_active')
PGWorkingset = 1 << prog.constant('PG_workingset')
PGWaiters = 1 << prog.constant('PG_waiters')
PGError = 1 << prog.constant('PG_error')
PGSlab = 1 << prog.constant('PG_slab')
PGOwner_priv_1 = 1 << prog.constant('PG_owner_priv_1')
PGArch_1 = 1 << prog.constant('PG_arch_1')
PGReserved = 1 << prog.constant('PG_reserved')
PGPrivate = 1 << prog.constant('PG_private')
PGPrivate_2 = 1 << prog.constant('PG_private_2')
PGWriteback = 1 << prog.constant('PG_writeback')
PGHead = 1 << prog.constant('PG_head')
PGMappedtodisk = 1 << prog.constant('PG_mappedtodisk')
PGReclaim = 1 << prog.constant('PG_reclaim')
PGSwapbacked = 1 << prog.constant('PG_swapbacked')
PGUnevictable = 1 << prog.constant('PG_unevictable')
PGMlocked = 1 << prog.constant('PG_mlocked')
PGUncached = 1 << prog.constant('PG_uncached')
PGHwpoison = 1 << prog.constant('PG_hwpoison')


def page_flags(page):
    flags = page.flags
    ret = []

    if flags & PGHead:
        ret += ['head']
    if flags & PGSlab:
        ret += ['slab']
    if flags & PGLocked:
        ret += ['locked']
    if flags & PGReferenced:
        ret += ['referenced']
    if flags & PGUptodate:
        ret += ['uptodate']
    if flags & PGDirty:
        ret += ['dirty']
    if flags & PGLru:
        ret += ['lru']
    if flags & PGActive:
        ret += ['active']
    if flags & PGWorkingset:
        ret += ['workingset']
    if flags & PGWaiters:
        ret += ['waiters']
    if flags & PGError:
        ret += ['error']
    if flags & PGSlab:
        ret += ['slab']
    if flags & PGOwner_priv_1:
        ret += ['owner_priv_1']
    if flags & PGArch_1:
        ret += ['arch_1']
    if flags & PGReserved:
        ret += ['reserved']
    if flags & PGPrivate:
        ret += ['private']
    if flags & PGPrivate_2:
        ret += ['private_2']
    if flags & PGWriteback:
        ret += ['writeback']
    if flags & PGHead:
        ret += ['head']
    if flags & PGMappedtodisk:
        ret += ['mappedtodisk']
    if flags & PGReclaim:
        ret += ['reclaim']
    if flags & PGSwapbacked:
        ret += ['swapbacked']
    if flags & PGUnevictable:
        ret += ['unevictable']
    if flags & PGMlocked:
        ret += ['mlocked']
    if flags & PGUncached:
        ret += ['uncached']
    if flags & PGHwpoison:
        ret += ['hwpoison']

    return ret


def for_each_online_node():
    online_mask = prog['node_states'][prog.constant('N_ONLINE')].bits[0]
    for nid in range(32):
        if online_mask & (1 << nid):
            yield prog['node_data'][nid]


def next_page(page):
    return Object(prog, page.type_, value=page + 1)


def PageHead(page):
    return page.flags & PGHead


def PageSlab(page):
    return page.flags & PGSlab


def PageHeadHuge(page):
    if PageHead(page):
        return next_page(page).compound_dtor == prog.constant('HUGETLB_PAGE_DTOR')
    else:
        return False


def PageBuddy(page):
    type = page.page_type & 0x7ff
    return type & 0x80


def compound_order(page):
    if PageHead(page):
        return int(next_page(page).compound_order)
    else:
        return 0


def draw_pages(pages, min_pfn, max_pfn):
    pages_per_block = 1024 * 1024 * 1024 // 4096
    block_size = 64
    blocks_in_row = 8

    width = block_size * blocks_in_row + 2 * (blocks_in_row - 1)
    blocks = math.ceil((max_pfn - min_pfn) / pages_per_block)
    height = block_size * math.ceil(blocks / blocks_in_row) * (blocks // blocks_in_row - 1)
    img = Image.new("RGB", (width, height))
    pixels = img.load()

    res = pages_per_block // block_size // block_size
    
    for pfn in range(min_pfn, max_pfn, res):
        block = pfn // pages_per_block
        off = (pfn % pages_per_block) // res

        block_x = (block % blocks_in_row)
        block_y = (block // blocks_in_row)

        x = block_size * block_x + 2 * block_x
        y = block_size * block_y + 2 * block_y

        x += off % block_size
        y += off // block_size

        color = [20, 20, 230]

        for i in range(res):
            if pfn + i >= max_pfn:
                break;
            if pages[pfn + i] == -1:
                color = [20, 20, 20]
                break
            elif pages[pfn + i] == 1:
                color = [215, 20, 20]
            elif pages[pfn + i] == 0:
                color[0] += 210 // res
                color[1] += 210 // res

        try:
            pixels[x, y] = tuple(color)
        except:
            print("x %d y %d block %d off %d color %s" % (x, y, block, off, color))
            raise

    img.save("mem.png")


def main():
    pages = {}

    min_pfn = 0
    max_pfn = 0

    for node in for_each_online_node():
        for zid in range(node.nr_zones):
            zone = node.node_zones[zid]

            if min_pfn == 0:
                min_pfn = int(zone.zone_start_pfn)

            if int(zone.zone_start_pfn) + int(zone.spanned_pages) > max_pfn:
                max_pfn = int(zone.zone_start_pfn + zone.spanned_pages)

    for pfn in range(min_pfn, max_pfn):
        try:
            page = cast('struct page *', _vmemmap(prog) + pfn)
            if PageSlab(page):
                pages[pfn] = 1
            elif PageBuddy(page):
                pages[pfn] = 0
            else:
                pages[pfn] = 2
        except:
            pages[pfn] = -1
        
    draw_pages(pages, min_pfn, max_pfn)


main()

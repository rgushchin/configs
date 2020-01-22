import sys, drgn
from drgn import NULL, Object, cast, container_of, execscript, reinterpret, sizeof
from drgn.helpers.linux import *
from drgn.helpers.linux.mm import for_each_page, pfn_to_page


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


def pages_to_GB(pages):
    return int(pages) * 4096 / 1024 / 1024 / 1024


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


def inspect_slab_page(page):
    print('      %s' % page.slab_cache.name.string_().decode('ascii'))


def inspect_page(page):
    if PageSlab(page):
        inspect_slab_page(page)
        return True

    if PageBuddy(page):
        return False

    return False
    # desc = page_flags(page)
    # desc += ['mapping %d' % page.mapping]
    # desc += ['type %d' % page.page_type]
    # desc += ['order %d' % compound_order(page)]
    # print(desc)

    
def inspect_pfn_range(zone, start_pfn, step):
    huge = False
    page = cast('struct page *', prog['vmemmap_base']) + start_pfn
    desc = ''

    if start_pfn + step > zone.zone_start_pfn + zone.present_pages:
        return False

    try:
        order = compound_order(page)
        huge = PageHeadHuge(page)

        if huge:
            if order == 18:
                desc = '1GB hugepage'
            elif order == 9:
                desc = '2MB hugepage'
            else:
                desc = 'unknown?? hugepage'

        print('    %-10d (%s)' % (start_pfn, desc))

        if not huge:
            n = 0
            for pfn in range(start_pfn, start_pfn + step):
                page = cast('struct page *', prog['vmemmap_base']) + pfn
                if inspect_page(page):
                    n += 1
                if n > 10:
                    break

    except drgn.FaultError as e:
        print('error: %s' % e)

    return huge


def main():
    # 1GB step
    step = 1024 * 1024 * 1024 // 4096
    # 2MB step
    #step = 2 * 1024 * 1024 // 4096
    found = 0
    total = -1

    for i in range(0, 2):
        if prog['hstates'][i].order == 18:
            total = prog['hstates'][i].nr_huge_pages

    for node in for_each_online_node():
        print('Node %d' % node.node_id)

        for zid in range(node.nr_zones):
            zone = node.node_zones[zid]

            managed = int(zone.managed_pages.counter.value_())
            if managed == 0:
                continue

            spanned = int(zone.spanned_pages)
            present = int(zone.present_pages)
            free = int(zone.vm_stat[prog.constant('NR_FREE_PAGES')].counter.value_())

            print('  Zone %s' % zone.name.string_().decode('ascii'))
            # print('        spanned %10d (%.1f GB)' % (spanned, pages_to_GB(spanned)))
            # print('        present %10d (%.1f GB)' % (present, pages_to_GB(present)))
            # print('        managed %10d (%.1f GB)' % (managed, pages_to_GB(managed)))

            # print('        free    %10d (%.1f Gb, %.1f%%)' %
            #       (free, pages_to_GB(free), free * 100 / managed))

            start = zone.zone_start_pfn
            if start % step:
                start = start + step - start % step
            
            for pfn in range(start, zone.zone_start_pfn + zone.present_pages,
                             step):
                if inspect_pfn_range(zone, pfn, step):
                    found += 1

    print('--')
    print('found %d of %d' % (found, total))


main()




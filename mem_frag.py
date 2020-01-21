import drgn
from drgn import NULL, Object, cast, container_of, execscript, reinterpret, sizeof
from drgn.helpers.linux import *
from drgn.helpers.linux.mm import for_each_page, pfn_to_page


PGHead = 1 << prog.constant('PG_head')


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


def PageHeadHuge(page):
    if PageHead(page):
        return next_page(page).compound_dtor == prog.constant('HUGETLB_PAGE_DTOR')
    else:
        return False


def compound_order(page):
    if PageHead(page):
        return int(next_page(page).compound_order)
    else:
        return 0

    
def inspect_pfn_range(zone, start_pfn, max_pfn):
    ret = False
    page = cast("struct page *", prog["vmemmap_base"]) + start_pfn

    try:
        order = compound_order(page)
        ret = PageHeadHuge(page)

        if ret:
            if order == 18:
                desc = "1GB hugepage"
            elif order == 9:
                desc = "2MB hugepage"
            else:
                desc = "unknown?? hugepage"
        else:
            for pfn in range(start_pfn, max_pfn):
                if PageMovable(page):

            desc = "()"

        print("    %-10d %s" % (start_pfn - zone.zone_start_pfn, desc))
    except:
        #print("bad pfn %s" % pfn)
        pass

    return ret


def main():
    # 1GB step
    pfn_step = 1024 * 1024 * 1024 // 4096
    # 2MB step
    # pfn_step = 2 * 1024 * 1024 // 4096
    found = 0
    total = -1

    for i in range(0, 2):
        if prog['hstates'][i].order == 18:
            total = prog['hstates'][i].nr_huge_pages

    for node in for_each_online_node():
        print("Node %d" % node.node_id)

        for zid in range(node.nr_zones):
            zone = node.node_zones[zid]

            managed = int(zone.managed_pages.counter.value_())
            if managed == 0:
                continue

            spanned = int(zone.spanned_pages)
            present = int(zone.present_pages)
            free = int(zone.vm_stat[prog.constant('NR_FREE_PAGES')].counter.value_())

            print("  Zone %s" % zone.name.string_().decode('ascii'))
            # print("        spanned %10d (%.1f GB)" % (spanned, pages_to_GB(spanned)))
            # print("        present %10d (%.1f GB)" % (present, pages_to_GB(present)))
            # print("        managed %10d (%.1f GB)" % (managed, pages_to_GB(managed)))

            # print("        free    %10d (%.1f Gb, %.1f%%)" %
            #       (free, pages_to_GB(free), free * 100 / managed))
            
            for pfn in range(zone.zone_start_pfn, zone.zone_start_pfn + zone.present_pages,
                             pfn_step):
                if inspect_pfn_range(zone, pfn, min(pfn + pfn_step,
                                                    zone.zone_start_pfn + zone.present_pages)):
                    found += 1

    print("--")
    print("found %d of %d" % (found, total))


main()




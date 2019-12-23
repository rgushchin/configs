import drgn
from drgn import NULL, Object, cast, container_of, execscript, reinterpret, sizeof
from drgn.helpers.linux import *
from drgn.helpers.linux.mm import for_each_page, pfn_to_page


def for_each_online_node():
    online_mask = prog['node_states'][prog.constant('N_ONLINE')].bits.value_()
    for nid in range(len(online_mask)):
        if online_mask[nid]:
            yield prog['node_data'][nid]


def pfn_present(pfn):
    PAGE_SHIFT = 12
    PFN_SECTION_SHIFT = 27 - PAGE_SHIFT

    section = prog['mem_section'][0][pfn >> PFN_SECTION_SHIFT]

    return (section.section_mem_map & 7) == 7


def main():
    for node in for_each_online_node():
        print("Node %d" % node.node_id)

        for zid in range(node.nr_zones):
            zone = node.node_zones[zid]

            managed = zone.managed_pages.counter.value_()
            spanned = zone.spanned_pages
            present = zone.present_pages
            free = zone.vm_stat[prog.constant('NR_FREE_PAGES')].counter.value_()

            print("    Zone %s" % zone.name.string_().decode('ascii'))
            print("        spanned %10d (%.1f Gb)" %
                  (spanned, spanned * 4096 / 1024 / 1024 / 1024))
            print("        present %10d (%.1f Gb)" %
                  (present, present * 4096 / 1024 / 1024 / 1024))
            print("        managed %10d (%.1f Gb)" %
                  (managed, managed * 4096 / 1024 / 1024 / 1024))

            # print("        free    %10d (%.1f Gb, %.1f%%)" %
            #       (free, free * 4096 / 1024 / 1024 / 1024, free * 100 / managed))

            processed = 0
            failed = 0
            nr2 = 0

            flags = {}
            
            for pfn in range(zone.zone_start_pfn, zone.zone_start_pfn + zone.present_pages):
                if not pfn_present(pfn):
                    continue

                processed += 1
                page = cast('struct page *', prog['vmemmap_base'] + pfn)
                try:
                    flags[pfn] = page.flags
                except:
                    failed += 1

            print("        processed %10d" % processed)
            print("        failed    %10d" % failed)



main()




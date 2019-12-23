#/bin/sh

KERNEL=arch/x86/boot/bzImage
ROOTFS="/dev/vda2"
CGROUPV2="cgroup_no_v1=all systemd.unified_cgroup_hierarchy=1"

qemu-system-x86_64 \
    -enable-kvm \
    -cpu host \
    -m 8192 \
    -drive if=virtio,file=/home/guro/vms/fedora.img,format=qcow2 \
    -no-reboot \
    -smp 2 \
    -device e1000,netdev=user.0 -netdev user,id=user.0,hostfwd=tcp::10022-:22 \
    -kernel ${KERNEL} \
    -append "root=${ROOTFS} ${CGROUPV2} console=ttyS0,115200" \
    -nographic


#-cdrom /home/guro/Downloads/Fedora-Server-dvd-x86_64-27-1.6.iso
#    -netdev user,id=user.0 -device e1000 \
#    -netdev=user.0,hostfwd=tcp::10022::22 \

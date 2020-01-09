#!/usr/bin/python3

import subprocess

mails={}


def grep_authors():
    proc = subprocess.run(['git', 'log', '-10000', '--format="%aN <%ae>"'], capture_output=True)
    raw = proc.stdout.decode('utf-8')

    for line in raw.split('\n'):
        parts = line.split('<')

        if len(parts) > 1:
            name = parts[0][1:-1]
            mail = parts[1][0:-2]
        else:
            continue

        #print("{%s}{%s}" % (name, mail))
        add_name_mail(name, mail)


def grep_log():
    proc = subprocess.run(['git', 'log', '-10000', '--grep=Cc:'], capture_output=True)
    raw = proc.stdout.decode('utf-8')

    for line in raw.split('\n'):
        line = line.lstrip()
        if not (line.startswith('Reviewed-by: ') or
                line.startswith('Acked-by: ')):
            continue

        line = line[line.find(': ') + 2:]
        name = line[:line.rfind(' ')]
        mail = line[line.rfind(' ') + 2:-1]

        if '@' not in mail:
            continue

        #print("{%s}{%s}" % (name, mail))
        add_name_mail(name, mail)


def add_name_mail(name, mail):
    if mail in mails:
        if name in mails[mail]:
            mails[mail][name] += 1
        else:
            mails[mail][name] = 1
    else:
        mails[mail] = {name: 1}


def gen_mutt_aliases():
    lines = []
    for mail in mails:
        aliases = sorted(mails[mail])

        if len(aliases[0]) == 0:
            lines.append("%s <%s>" % (mail, mail))
        else:
            short = '_'.join(aliases[0].split(' ')).lower()
            lines.append("%s %s <%s>" % (short, aliases[0], mail))

    for line in sorted(lines):
        print("alias %s" % line)


if __name__ == '__main__':
    grep_log()
    grep_authors()
    gen_mutt_aliases()

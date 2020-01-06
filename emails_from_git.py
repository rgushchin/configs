#!/usr/bin/python3

import subprocess

mails={}
names={}


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

        add_name_mail(name, mail)


def grep_cc():
    proc = subprocess.run(['git', 'log', '-10000', '--grep=Cc:'], capture_output=True)
    raw = proc.stdout.decode('utf-8')

    for line in raw.split('\n'):
        line = line.lstrip()
        if not line.startswith('Cc: '):
            continue
        if '@' not in line:
            continue

        if '<' in line:
            line = line[4:line.find('>')]    
            parts = line.split('<')

            if len(parts) > 1:
                name = parts[0].strip()
                mail = parts[1].strip()

        else:
            parts = line.split(' ')
            for part in parts:
                if '@' in part:
                    name = ""
                    mail = part
                    break

        add_name_mail(name, mail)


def add_name_mail(name, mail):
    if mail in mails:
        if name in mails[mail]:
            mails[mail][name] += 1
        else:
            mails[mail][name] = 1
    else:
        mails[mail] = {name: 1}

    if name in names:
        if mail in names[name]:
            names[name][mail] += 1
        else:
            names[name][mail] = 1
    else:
        names[name] = {mail: 1}


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
    grep_cc()
    grep_authors()
    gen_mutt_aliases()

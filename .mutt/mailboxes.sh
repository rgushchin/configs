maildir=/home/guro/.mail/fb

# Find these specific paths so they stick at the top of the list
find "${maildir}" -maxdepth 2 -type d -name cur     \
    -a      \( -ipath "*inbox*"                     \
            -o -ipath "*sent*"                      \
            -o -ipath "*drafts*"                    \
            -o -ipath "*trash*"                     \
            \)                                      \
    | sed                                           \
        -e "s#^${maildir}/#mailboxes \"+#"          \
        -e 's#/cur$#"#'                             \
    | sort                                          \
    > "${maildir}/mailboxes"

# Find all other paths, excluding those from above
find "${maildir}" -maxdepth 2 -type d -name cur     \
    -a -not \( -ipath "*inbox*"                     \
            -o -ipath "*sent*"                      \
            -o -ipath "*drafts*"                    \
            -o -ipath "*trash*"                     \
            \)                                      \
    | sed                                           \
        -e "s#^${maildir}/#mailboxes \"+#"          \
        -e 's#/cur$#"#'                             \
    | sort                                          \
    >> "${maildir}/mailboxes"

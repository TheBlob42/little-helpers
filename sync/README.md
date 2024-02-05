# Sync

Small little helper for `rsync`

- bundles multiple `rsync` operations
- reads the directories to sync from an `edn` configuration file
- shows a "colorful" diff in `less` and asks for operation confirmation

For example:

```bash
sync.clj backup.edn /media/user/external-hard-drive
```

The configuration file contains a vector of maps which each specify the following options:

- `source`: The source directory that should be copied
- `target`: The path on the targeted main folder (e.g. `/media/user/stick/<target>`)
- `mode`: Should new files only be added (`:add`) or should non existing files also be deleted (`:delete`)
    - The `:delete` mode is equivalent to the `--delete` option of `rsync`

Example configuration:

```clojure
[{:source "~/Documents"
  :target "Data/Documents"
  :mode :delete}
 {:source "~/Pictures"
  :target "Media/Pictures"
  :mode :add}]
```

## Dependencies

- [babashka](https://github.com/babashka/babashka)
- [rsync](https://man7.org/linux/man-pages/man1/rsync.1.html)
- [less](https://man7.org/linux/man-pages/man1/less.1.html)

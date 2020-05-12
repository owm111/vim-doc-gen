# vim-doc-gen

Super basic Vim documentation generatation tool.

This project is unfinished; lacks many features and has a messy implemenation.

- [ ] Output customization.
- [ ] Support `function!`.
- [ ] Optional function arguments.

## Usage

There are three "modes" included: functions, commands, and syntax. These generate documentation for functions, commands, and highlight groups, respectively.

    $ vim-doc-gen functions < autoload/myfile.vim
    $ vim-doc-gen commands < ftplugin/myfile.vim
    $ vim-doc-gen syntax < syntax/myfile.vim

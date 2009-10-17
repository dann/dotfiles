if exists("b:did_YAML_ftplugin")
  finish
endif
let b:did_YAML_ftplugin = 1

compiler yaml
setlocal ts=2 sw=2 sts=2

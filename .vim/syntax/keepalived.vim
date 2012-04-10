" Vim syntax file
" Language:     keepalived config http://www.keepalived.org/
" URL:          https://github.com/glidenote/keepalived-syntax.vim
" Version:      1.0.0
" Maintainer:   Akira Maeda <glidenote@gmail.com>

if exists("b:current_syntax")
  finish
end

setlocal iskeyword+=.
setlocal iskeyword+=/
setlocal iskeyword+=:

syn match   keepalivedDelimiter   "[{}()\[\];,]"
syn match   keepalivedOperator    "[~!=|&\*\+\<\>]"
syn match   keepalivedComment     "\(#.*\)"
syn match   keepalivedNumber      "[-+]\=\<\d\+\(\.\d*\)\=\>"
syn region  keepalivedString      start=+"+ skip=+\\"+ end=+"+
syn region  keepalivedBlock start=+^+ end=+{+ contains=keepalivedComment,keepalivedDefinitionBlock,keepalivedDefinitionImportant,keepalivedDefinition oneline

syn keyword keepalivedBoolean on
syn keyword keepalivedBoolean off

syn keyword keepalivedDefinitionBlock global_defs         contained
syn keyword keepalivedDefinitionBlock virtual_server      contained
syn keyword keepalivedDefinitionBlock fwmark              contained
syn keyword keepalivedDefinitionBlock vrrp_sync_group     contained
syn keyword keepalivedDefinitionBlock vrrp_instance       contained

syn keyword keepalivedDefinitionImportant notification_email
syn keyword keepalivedDefinitionImportant notification_email_from
syn keyword keepalivedDefinitionImportant smtp_server
syn keyword keepalivedDefinitionImportant smtp_connect_timeout
syn keyword keepalivedDefinitionImportant lvs_id
syn keyword keepalivedDefinitionImportant delay_loop
syn keyword keepalivedDefinitionImportant lb_algo
syn keyword keepalivedDefinitionImportant lb_kind
syn keyword keepalivedDefinitionImportant lvs_sched
syn keyword keepalivedDefinitionImportant lvs_method
syn keyword keepalivedDefinitionImportant persistence_timeout
syn keyword keepalivedDefinitionImportant persistence_granularity
syn keyword keepalivedDefinitionImportant virtualhost
syn keyword keepalivedDefinitionImportant protocol
syn keyword keepalivedDefinitionImportant sorry_server
syn keyword keepalivedDefinitionImportant real_server
syn keyword keepalivedDefinitionImportant state
syn keyword keepalivedDefinitionImportant interface
syn keyword keepalivedDefinitionImportant mcast_src_ip
syn keyword keepalivedDefinitionImportant lvs_sync_daemon_inteface
syn keyword keepalivedDefinitionImportant virtual_router_id
syn keyword keepalivedDefinitionImportant priority
syn keyword keepalivedDefinitionImportant advert_int
syn keyword keepalivedDefinitionImportant smtp_alert
syn keyword keepalivedDefinitionImportant authentication
syn keyword keepalivedDefinitionImportant virtual_ipaddress
syn keyword keepalivedDefinitionImportant virtual_ipaddress_excluded
syn keyword keepalivedDefinitionImportant notify_master
syn keyword keepalivedDefinitionImportant notify_backup
syn keyword keepalivedDefinitionImportant notify_fault
syn keyword keepalivedDefinitionImportant vrrp_sync_group

syn keyword keepalivedDefinition weight
syn keyword keepalivedDefinition TCP_CHECK
syn keyword keepalivedDefinition MISC_CHECK
syn keyword keepalivedDefinition HTTP_GET
syn keyword keepalivedDefinition SSL_GET
syn keyword keepalivedDefinition url
syn keyword keepalivedDefinition connect_port
syn keyword keepalivedDefinition connect_timeout
syn keyword keepalivedDefinition nb_get_retry
syn keyword keepalivedDefinition delay_before_retry
syn keyword keepalivedDefinition router_id
syn keyword keepalivedDefinition state
syn keyword keepalivedDefinition interface
syn keyword keepalivedDefinition virtual_router_id
syn keyword keepalivedDefinition priority
syn keyword keepalivedDefinition advert_int
syn keyword keepalivedDefinition nat_mask
syn keyword keepalivedDefinition auth_type
syn keyword keepalivedDefinition auth_pass

syn keyword keepalivedVariable  misc-path
syn keyword keepalivedVariable  path
syn keyword keepalivedVariable  digest

" highlight
hi link keepalivedDelimiter           Delimiter
hi link keepalivedOperator            Operator
hi link keepalivedComment             Comment
hi link keepalivedNumber              Number
hi link keepalivedComment             Comment
hi link keepalivedVariable            PreProc
hi link keepalivedBlock               Normal
hi link keepalivedString              String

hi link keepalivedBoolean             Boolean
hi link keepalivedDefinitionBlock     Statement
hi link keepalivedDefinitionImportant Type
hi link keepalivedDefinition          Identifier

let b:current_syntax = "keepalived"

#!/usr/bin/expect

set password Vicky20130529

spawn ssh rarnu@7thgen.info "ls"  "-la"  "~/" 
expect {

   "password:" { set timeout 500; send "$password\r" }
   "yes/no" { set timeout 500; send "yes\r"; exp_continue; }
}

expect eof


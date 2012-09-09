#! /usr/bin/env python

descr = """ 
    witsproxy 
    Sets the Wits proxy for all the files that I have to 
    to set the proxy for 
    
    author: Leonard Mandla Mbuli <lm.mbuli@gmail.com>
    
    creation date: 08 July 2011
    last update: 8 September 2012
    
    python 3
"""

import argparse
import re
import os
import sys
import fileinput
import textwrap

def main():
    """ The main function"""
    parser = argparse.ArgumentParser(
            formatter_class=argparse.RawDescriptionHelpFormatter,
            description=textwrap.dedent(descr)
            ) 
    parser.add_argument('username',
                    help="Your student or staff number, without ds\\ or students\\")
    parser.add_argument('password', 
                    help="The new or updated proxy password")
    parser.add_argument('-a', '--set_apt', action='store_true', 
                    help="Set the apt proxy (Ubuntu or the like)")
    parser.add_argument('-b', '--set_bash', action='store_true', 
                    help="Set the proxy in ~/.bashrc")
    parser.add_argument('-c', '--set_current_shell', action='store_true', 
                    help="Exports {http,ftp,https}_proxy variables in current shell")
    parser.add_argument('-z', '--set_zsh', action='store_true', 
                    help="Set the proxy in ~/.zshrc (TO BE DONE)")
    parser.add_argument('-s', '--set_svn', action='store_true', 
                    help="Set the proxy in ~/.subversion/servers (TO BE DONE)")
    parser.add_argument('-C', '--use_cntlm', action='store_true', 
                    help="Use CNTML to set the proxy (CNTML must installed but not configured)")
    parser.add_argument('-S', '--is_staff', action='store_true', 
                    help="Change proxy to staff proxy")
    args = parser.parse_args()

    proxystr,proxyaddress = make_proxystr(args.username,args.password,args.use_cntlm,args.is_staff)
    if(args.use_cntlm):
        set_cntlm_config(args.username,args.password,proxyaddress,args.is_staff);
    if args.set_bash:
        set_bash_proxy(proxystr)
    if args.set_svn:
        set_svn_proxy(args.username,args.password,proxyaddress,args.is_staff)
    if args.set_apt:
        set_apt_proxy(proxystr)

def make_proxystr(username,password,cntlm,staff):
    proxyaddress = "proxyss.wits.ac.za:80"
    if(staff):
        proxyaddress = "proxyad.wits.ac.za:80"
        proxystr = "http://ds\\"+username+":"+password+"@"+proxyaddress
    elif(cntlm):
        proxystr += "127.0.0.1:8000"
    else:
        proxystr = "http://students\\"+username+":"+password+"@"+proxyaddress
    return proxystr,proxyaddress

def set_apt_proxy(proxystr):
    """ 
    Sets the proxy for apt so I can install stuff
    Overides the set file
    """
    http_proxy = "Acquire::Proxy::Http \""+proxystr+"\";\n"
    https_proxy = "Acquire::Proxy::Https \""+proxystr+"\";\n"
    ftp_proxy = "Acquire::Proxy::Ftp \""+proxystr+"\";\n"

    f = open('/etc/apt/apt.conf', 'w')
    f.write(http_proxy)
    f.write(https_proxy)
    f.write(ftp_proxy)
    f.close()

def set_cntlm_config(username,password,address,is_staff):
    """ 
        Set the proxy details in the cntlm config 
    """
    domain = "students"
    if(is_staff):
        domain = "ds"
    no_proxy="*wits.ac.za,*mirror.ac.za"
    config_file = "/etc/cntlm.conf"

    if os.path.isfile(config_file):
        replacements = 0
        for line in fileinput.input(config_file, inplace = True):
            if re.match(r'Username', line)!= None:
                replacements = replacements + 1
                sys.stdout.write('{0:11} {1}\n'.format("Username",username))
            elif re.match(r'Domain', line)!= None:
                sys.stdout.write('{0:11} {1}\n'.format("Domain",domain))
                replacements = replacements + 1
            elif re.match(r'Password', line)!= None:
                sys.stdout.write('{0:11} {1}\n'.format("Password",password))
                replacements = replacements + 1
            elif re.match(r'Proxy', line)!= None:
                sys.stdout.write('{0:11} {1}\n'.format("Proxy",address))
                replacements = replacements + 1
            elif re.match(r'NoProxy', line)!= None:
                line = line.rstrip("\n")
                sys.stdout.write(line+","+no_proxy+"\n")
                replacements = replacements + 1
            elif re.match(r'Listen', line)!= None:
                sys.stdout.write('{0:11} {1}\n'.format("Listen","8000"))
                replacements = replacements + 1
            else:
                sys.stdout.write(line)
        if replacements == 0:
           f = open(config_file, 'a')
           details = {"Username":username,"Domain":domain,"Password":password,"Proxy":address,"Listen":"8000"}
           for detail,value in details.items():
               f.write('{0:11} {1}\n'.format(detail,value))
           f.close()
           print("Proxy settings not found, thus appended")
        elif replacements == 5:
           print("Proxy settings successfully changed")
    else:
        f = open(config_file, 'w')
        details = {"Username":username,"Domain":domain,"Password":password,"Proxy":address,"Listen":"8000"}
        for detail,value in details.items():
            f.write('{0:11} {1}\n'.format(detail,value))
        f.close()
        print("Created new '"+config_file+"' with new proxy settings")

def set_bash_proxy(proxystr):
    """ 
      Set the proxy in the .bashrc file

      - if does then replace them with the new ones
      - if it does not have the proxy str then append the string with message
      - if bashrc does not exist make it and append 
    """
    http_proxy = "export http_proxy=\""+proxystr+";\"\n"
    https_proxy = "export https_proxy=\""+proxystr+";\"\n"
    ftp_proxy = "export ftp_proxy=\""+proxystr+";\"\n"
    bashrc = os.path.join(os.getenv("HOME"), ".bashrc")

    if os.path.isfile(bashrc):
        replacements = 0
        for line in fileinput.input(bashrc, inplace = True):
            if re.match(r'export http_proxy=', line)!= None:
                replacements = replacements + 1
                sys.stdout.write(http_proxy)
            elif re.match(r'export https_proxy=', line)!= None:
                sys.stdout.write(https_proxy)
                replacements = replacements + 1
            elif re.match(r'export ftp_proxy=', line)!= None:
                sys.stdout.write(ftp_proxy)
                replacements = replacements + 1
            else:
                sys.stdout.write(line)
        if replacements == 0:
           f = open(bashrc, 'a')
           f.write(http_proxy)
           f.write(https_proxy)
           f.write(ftp_proxy)
           f.close()
           print("Proxy settings not found, thus appended")
        elif replacements == 3:
           print("Proxy settings successfully changed")
    else:
        f = open(bashrc, 'w')
        f.write(http_proxy)
        f.write(https_proxy)
        f.write(ftp_proxy)
        f.close()
        print("Created new '"+bashrc+"' with new proxy settings")

def set_svn_proxy(username,passowrd,proxyaddress,is_staff):
    """ 
      Set the proxy in the .subversion/servers file

      - if does then replace them with the new ones
      - if it does not have the proxy str then append the string with message
      - if svn_file file does not exist make it and append 
    """
    host = proxyaddress[:-3]
    port = proxyaddress[-2:]
    domain = "students\\"
    if (is_staff):
        domain = "ds\\"
    print(host,port)
    svn_file = os.path.join(os.getenv("HOME"), ".subversion/svn_file")

    if os.path.isfile(svn_file):
        replacements = 0
        for line in fileinput.input(svn_file, inplace = True):
            if re.match(r'http-proxy-host = ', line)!= None:
                replacements = replacements + 1
                sys.stdout.write("http-proxy-host = "+host)
            elif re.match(r'http-proxy-port = ', line)!= None:
                replacements = replacements + 1
                sys.stdout.write("http-proxy-port = "+port)
            elif re.match(r'http-proxy-username = ', line)!= None:
                replacements = replacements + 1
                sys.stdout.write("http-proxy-username = "+domain+username)
            elif re.match(r'http-proxy-password = ', line)!= None:
                replacements = replacements + 1
                sys.stdout.write("http-proxy-password = "+password)
            else:
                sys.stdout.write(line)
    #     if replacements == 0:
    #        f = open(svn_file, 'a')
    #        f.write(http_proxy)
    #        f.write(https_proxy)
    #        f.write(ftp_proxy)
    #        f.close()
    #        print("Proxy settings not found, thus appended")
    #     elif replacements == 3:
    #        print("Proxy settings successfully changed")
    # else:
    #     f = open(svn_file, 'w')
    #     f.write(http_proxy)
    #     f.write(https_proxy)
    #     f.write(ftp_proxy)
    #     f.close()
    #     print("Created new '"+svn_file+"' with new proxy settings")

if __name__ == '__main__':
    main()

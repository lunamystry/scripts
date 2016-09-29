#! /usr/bin/env python
import click
import urllib2
import os
import shutil
import tarfile
import zipfile

version = "r24.0.2"
base_url = "http://dl.google.com/android/"
home = os.path.expanduser("~")
install_directory = os.path.join(home, ".usr/local/android-sdk/")
shell_rcfile = os.path.join(home, ".zsh/exports.zsh")
os_name = 'linux'
download_directory = '/tmp/android-sdk/'


def download(url, filename):
    # TODO: What if I can't download the file
    sdk_download = urllib2.urlopen(url)
    # TODO: What if the directory is not valid
    f = open(filename, 'wb')
    meta = sdk_download.info()
    file_size = int(meta.getheaders("Content-Length")[0])
    print("Downloading: %s Bytes: %s" % (filename, file_size))

    file_size_dl = 0
    block_sz = 8192
    # TODO: don't donwload if already exists
    while True:
        buffer = sdk_download.read(block_sz)
        if not buffer:
            break

        file_size_dl += len(buffer)
        f.write(buffer)
        status = r"%10d  [%3.2f%%]" % (file_size_dl, file_size_dl * 100. / file_size)
        status = status + chr(8)*(len(status)+1)
        print(status)

    f.close()


@click.command()
@click.option('--base_url', default=base_url, help='The base URL to download the SDK from (with end /)')
@click.option('--version', default=version, help='The version of the SDK')
@click.option('--download_directory', default=download_directory, help='The place to download the files temporarly (with end /)')
@click.option('--install_directory', default=install_directory, help='where to install, must writable by the script')
@click.option('--shell_rcfile', default=shell_rcfile, help='Where the PATH is going to be appended to to allow running of sdk tools')
@click.option('--os_name', default=os_name, help='linux or macosx')
@click.option('--force', default=True, help='Force download even if file by same name exists')
def install(base_url, version, download_directory, install_directory, shell_rcfile, os_name, force):
    if os_name == 'linux':
        url_tail = "android-sdk_%s-linux.tgz" % version
    elif os_name == 'macosx':
        url_tail = "android-sdk_%s-macosx.zip" % version
    else:
        click.echo("Unkown OS, don't know how to download the SDK")
        return

    if not os.path.exists(download_directory):
        os.makedirs(download_directory)

    filename = os.path.join(download_directory, url_tail)

    if not os.path.exists(os.path.join(download_directory, filename)) and \
            not force:
        download(os.path.join(base_url,url_tail))

    if not os.path.exists(install_directory):
        os.makedirs(install_directory)

    if zipfile.is_zipfile(filename):
        with zipfile.ZipFile(filename, 'r') as z:
            z.extractall(install_directory)

    if tarfile.is_tarfile(filename):
        with tarfile.open(filename, 'r') as t:
            t.extractall(install_directory)

    with open(shell_rcfile, 'a') as rcfile:
        rcfile.write(r"export PATH=$PATH:%s" % os.path.join(install_directory, 'tools'))

    shutil.rmtree(download_directory)


if __name__ == '__main__':
    install()

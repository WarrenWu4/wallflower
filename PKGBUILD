# Maintainer: Warren Wu <warrenweiwu04@gmail.com>
pkgname=wallflower
pkgver=0.0.1
pkgrel=1
pkgdesc="gui wallpaper manager for hyprland"
arch=('x86_64')
url="https://github.com/WarrenWu4/wallflower"
license=('MIT')
depends=('hyprland' 'hyprpaper' 'zenity' 'libx11' 'mesa')
makedepends=('git' 'gcc' 'make')
source=("git+https://github.com/WarrenWu4/wallflower.git")
sha256sums=('SKIP')
options=('!debug' '!strip')

build() {
    cd "$pkgname"
    make prod
}

package() {
    cd "$pkgname"
    
    # install binary to /usr/bin
    mkdir -p "$pkgdir/usr/bin/"
    install -Dm755 build/main "$pkgdir/usr/bin/$pkgname"

    # move resources to /usr/share/wallflower
    mkdir -p "$pkgdir/usr/share/$pkgname/"
    cp -r resources/* "$pkgdir/usr/share/$pkgname/"

    # add desktop entry in /usr/share/applications
    mkdir -p "$pkgdir/usr/share/applications/"
    cp wallflower.desktop "$pkgdir/usr/share/applications/"

    # set app icon in /usr/share/icons/hicolor/...
    mkdir -p "$pkgdir/usr/share/icons/hicolor/64x64/"
    cp wallflower.png "$pkgdir/usr/share/icons/hicolor/64x64/"
}

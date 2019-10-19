# Expect

![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg?style=for-the-badge)
![GitHub issues](https://img.shields.io/github/issues-raw/jeblad/Expect?style=for-the-badge)

This [extension for Mediawiki](https://www.mediawiki.org/wiki/Extension:Expect) adds extended expectations (a kind of assertions) to Lua modules provided by the [Scribunto extension](https://www.mediawiki.org/wiki/Extension:Scribunto). An integral part is to report failures clearly and visible to facilitate interactive and collaborative fault fixing.

Extensive help is available at [Expect: Assertion framework for Lua embedded within Mediawiki](https://jeblad.github.io/Expect/), with a programmers guide, a reference, and examples.

## Usage

Expect depends on modules from the Scribunto extension.

1. Download from [Github](https://github.com/jeblad/Expect) ([zip](https://github.com/jeblad/Expect/archive/master.zip)) and place the file(s) in a directory called Expect in your extensions/ folder.
2. Add the following code at the bottom of your LocalSettings.php:

	```lua
	wfLoadExtension( 'Expect' );
	```

3. Done – Navigate to Special:Version on your wiki to verify that the extension is successfully installed.

## Development

Expect uses [Mediawiki-Vagrant](https://www.mediawiki.org/wiki/MediaWiki-Vagrant), and a complete setup can be made quite easily.

1. Make sure you have Vagrant, etc, prepare a development directory, and move to that directory.
2. Clone Mediawiki

	```bash
	git clone --recursive https://gerrit.wikimedia.org/r/mediawiki/vagrant .
	```

3. Add the role unless [#535661](https://gerrit.wikimedia.org/r/#/c/mediawiki/vagrant/+/535661/) has been merged. (You need [git-review](https://www.mediawiki.org/wiki/Gerrit/git-review) to do this.)

	```bash
	git review -d 535661
	```

4. Run setup.

	```bash
	./setup.sh
	```

5. Enable role for Expect. This pulls in the role for Scribunto, which then pulls in additional roles.

	```bash
	vagrant roles enable expect
	```

6. Start the instance.

	```bash
	vagrant up
	```

7. Done.

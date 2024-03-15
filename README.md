# prolog-etudes

[![CircleCI](https://dl.circleci.com/status-badge/img/circleci/UMKeFZ8ns9T9vi5aquTfVT/GmMZi7fYoEtn4APsVdADde/tree/main.svg?style=svg&circle-token=357dc9c7b5626a1481100a8b3128a552a29def93)](https://dl.circleci.com/status-badge/redirect/circleci/UMKeFZ8ns9T9vi5aquTfVT/GmMZi7fYoEtn4APsVdADde/tree/main)
[![codecov](https://codecov.io/gh/bahmanm/prolog-etudes/graph/badge.svg?token=R5KPMGJAOY)](https://codecov.io/gh/bahmanm/prolog-etudes) 
![GitHub commit activity](https://img.shields.io/github/commit-activity/m/bahmanm/prolog-etudes?style=flat&logo=github&label=commits)
[![Matrix](https://img.shields.io/matrix/github-bahmanm-prolog-etudes%3Amatrix.org?server_fqdn=matrix.org&style=social&logo=matrix)](https://matrix.to/#/#github-bahmanm-prolog-etudes:matrix.org)

A collection of etudes for folks new to Prolog (and sometimes experienced ones) to learn the
language in a practical fashion.
<img alt="prolog-etudes logo" src="https://imgur.com/pBBRDzr.png" style="height: 300px; width: 300px; vertical-align: top" /> 

# How To Run/Test

Assuming you've got all the necessary tooling installed (see below), all you'd need to is

```
＄ make test

...
% All 22 tests passed in 0.055 seconds (0.053 cpu)
```

### Prerequisites

To run the tests all you need to do is a recent SWI-Prolog and the latest
[bmakelib](https://github.com/bahmanm/bmakelib).

For example on an openSUSE Tumbleweed machine:

```
# zypper refresh && zypper install make swipl
# rpm --install --nosignature https://github.com/bahmanm/bmakelib/releases/download/v0.7.0/bmakelib-0.7.0-1.1.noarch.rpm
```

# CI Pipeline Setup 
_TBD_

# Test Code Coverage Setup
_TBD_

# nightfall

<a href="https://github.com/qredo/nightfall/blob/main/LICENSE"><img src="https://img.shields.io/badge/license-Apache--2.0-blue"></a>

A Haskell eDSL for targeting different zk-VMs.


**WARNING**:
This project is work in progress and subject to frequent changes.
It has not been audited and may contain bugs and security flaws. This implementation is NOT ready for production use.

---

## Table of Contents

1. [Getting Started](#getting-started)
   * [Prerequisites](#prerequisites)
2. [Usage](#usage)
3. [Contributing](#contributing)
4. [License](#license)
<!--5. [Acknowledgements](#acknowledgements)-->

---

## Getting Started

These instructions will help you set up and run nightfall on your local machine for development and testing purposes.

### Prerequisites

Before you begin, ensure you have the following software installed on your system:

1. [nix](https://nixos.org/download.html) (version 2.15.0 or later)

### Examples

Examples of programs described in the eDSL can be found in [the Examples folder](./src/Examples/).

### Usage

For now, the [Main](./app/Main.hs) is simply a dummy program to test the [examples](./src/Examples/). Launch with
```sh
cabal run nightfall -- <example-name>
```

to get the MASM output on stdin, or

```sh
cabal run nightfall -- <example-name> <filepath>
```

You can then run the generated miden programs with

```sh
miden run -a <path-to-your-generated-masm-file>
```

to save the MASM output on file (warning: override).

This will obviosuly disappear when the library gets more mature.

*Note*: this is first draft / alpha / experimental / subject to breaking changed even in the same day!
Types aren't safe, "smart constructors" aren't "smart". This is just to kickstart the project

### Contributing
We welcome contributions to nightfall! To contribute, please follow these steps:

* Fork the repository and clone it locally.
* Create a new branch (`git checkout -b feature/YourFeature`)
* Commit your changes (`git commit -m 'Add YourFeature'`)
* Push to the branch (`git push origin feature/YourFeature`)
* Create a Pull Request
* Please read `CONTRIBUTING.md` for details on our code of conduct and the process for submitting pull requests.

<!-- ## Acknowledgements -->

## Licence

This project is released under the terms of the Business Source License - see the `LICENSE`(LICENSES/FusionLtd-Bsl-License.txt) file for details.
The repository is [REUSE](https://reuse.software) compliant. The copyright owner are listed in the `.reuse/dep5` file or in the respective copyright notice.


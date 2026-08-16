Installation and Building
=========================

The OpenUxAS build and dependency workflow changes as supported operating systems and third-party dependencies evolve. The repository root `README.md` is the canonical source for current platform requirements, installation steps, and build commands.

Do not use the legacy Meson/Ninja installation instructions that were previously reproduced in this user manual. OpenUxAS now provides the `anod` command to obtain and build the required dependencies and OpenUxAS itself.

Quick start
-----------

1. Check the [repository README](../../../../README.md) for the currently supported operating systems and prerequisite tools.

2. Clone OpenUxAS and enter the repository:

    ```bash
    git clone https://github.com/afrl-rq/OpenUxAS
    cd OpenUxAS
    ```

3. Fetch dependencies and build OpenUxAS:

    ```bash
    ./anod build uxas
    ```

4. If OpenAMASE simulation support is required, build it with:

    ```bash
    ./anod build amase
    ```

5. Run an example, for example:

    ```bash
    ./run-example 02_Example_WaterwaySearch
    ```

Developing OpenUxAS
-------------------

Run the `anod` build first so that the repository-local third-party dependencies are available. After that initial build, the repository Makefile can be used for incremental OpenUxAS builds:

```bash
make -j all
```

If development also requires changes to LmcpGen or OpenAMASE, use the development setup commands documented in the root `README.md`, for example:

```bash
./anod devel-setup lmcp
./anod devel-setup amase
```

Running tests
-------------

After OpenUxAS has been built, the C++ test suite can be run from `tests/cpp`:

```bash
cd tests/cpp
./run-tests
```

See `tests/cpp/README.md` for test-development guidance.

Keeping installation guidance current
--------------------------------------

The root `README.md` should be updated whenever supported platforms, prerequisite tools, dependency management, or build commands change. This user-manual page intentionally points to that canonical workflow rather than duplicating platform-specific package lists that can become stale independently.

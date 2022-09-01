"""Module setup for uxas infrastructure."""
from setuptools import setup, find_packages

install_requires = [
    "e3-core==22.1.0",
    "e3-testsuite==24.0",
    "pyzmq",
]

setup(
    name="uxas",
    version=0.1,
    url="https://github.com/afrl-rq/OpenUxAS",
    licence="GLPv3",
    author="AFRL",
    author_email="aiello@adacore.com",
    description="Infrastructure support for OpenUxAS",
    namespace_packages=["uxas"],
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    install_requires=install_requires,
)

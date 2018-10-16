from setuptools import setup

def readme():
    with open("README.rst") as f:
        return f.read()

setup(
    name="bitwise",
    version="0.1.0.1",
    description="Bitwise is a library for utilizing Python as a hardware description language",
    long_description=readme(),
    classifiers=[
        "Development Status :: 3 - Alpha",
        "License :: OSI Approved :: MIT License",
        "Natural Language :: English",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.6",
        "Topic :: Scientific/Engineering",
        "Topic :: Software Development :: Libraries :: Python Modules"
    ],
    keywords="bitwise hardware design",
    url="https://github.com/jamesjiang52/Bitwise",
    author="James Jiang",
    author_email="jamesjiang52@gmail.com",
    license="MIT",
    packages=["bitwise"],
    install_requires=[],
    include_package_data=True,
    zip_safe=False
)

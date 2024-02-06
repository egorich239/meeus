from skbuild import setup

setup(
    name="meeus",
    version="0.0.1",
    description="a library of astronomical algorithms",
    author="Ivan Egorov",
    license="Apache 2.0",
    packages=["meeus"],
    python_requires=">=3.7",
    cmake_with_sdist = True,
    package_dir = {"": "python"},
    package_data = {"meeus": ["_meeus_c_impl.so"]},
    zip_safe = False,
)
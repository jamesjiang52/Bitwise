#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from collections import OrderedDict

extensions = []
templates_path = ["_templates"]
source_suffix = ".rst"
master_doc = "index"
language = "en"
exclude_patterns = ["_build"]

project = "Bitwise"
copyright = "2018, James Jiang"
author = "James Jiang"
version = "0.1"
release = "0.1.1"

nav_links = OrderedDict()
nav_links["Home"] = "index.html"
nav_links["Getting Started"] = "install.html"
nav_links["API Documentation"] = "api.html"
nav_links["Changelog"] = "changelog.html"
nav_links["View on Github"] = "https://github.com/jamesjiang52/Bitwise"

html_theme = "alabaster"
highlight_language="python3"
pygments_style = "lovelace"
html_theme_options = {
    "description": "Python library for hardware design",
    "fixed_sidebar": True,
    "logo": "logo.png",
    "logo_name": False,
    "page_width": "70%",
    "sidebar_width": "240px",
    "sidebar_collapse": True,
    "sidebar_includehidden": False,
    "show_powered_by": False,
    "extra_nav_links": nav_links
}
html_favicon = "images/favicon.ico"
html_static_path = ["_static", "_static/logo.png"]
html_sidebars = {
    "**": [
        "about.html",
        "navigation.html",
        "localtoc.html",
        "searchbox.html"
    ]
}
html_scaled_image_link=False
html_add_permalinks=None

htmlhelp_basename = "bitwisedoc"

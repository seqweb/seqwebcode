#!/usr/bin/env python3
"""
Generate Cursor workspace configuration for SeqWeb

We want to generate a Cursor config that looks something like this (JSON)

    {
        "folders": [
            {
                "name": "seqwebdev (home)",
                "path": "/Users/mlb/Devo/seqweb"
            },
           {
                "name": "seqwebcode (main)",
                "path": "/Users/mlb/Devo/seqweb/seqwebcode"
            },
            # *** more folder decls *** #
         ],
        "settings": {
            "files.exclude": {
                # seqwebdev-specific data dir entries
                "**/data/output": true,
                "**/data/intermediate": true,
                "**/data/logs": true
                # pre-existing user-defined entries
                "**/tmp": True,
                # *** more files.exclude patterns ***
            },
            "search.exclude": {
                # seqweb-specific repo dir entries
                f"{seqwebdata_path}/**": True,
                f"{oeisdata_path}/**": True,
                # seqwebdev-specific data dir entries
                "**/data/output/**": True,
                "**/data/intermediate/**": True,
                # pre-existing user-defined entries
                "**/tmp/**": True,
                # *** more files.exclude patterns ***
            },
            "files.watcherExclude": {
                # seqweb-specific repo dir entries
                "**/seqwebdata": true,
                "**/oeisdata": true,
                # seqwebdev-specific data dir entries
                "**/data/output": true,
                "**/data/intermediate": true,
                "**/data/logs": true
                # *** more files.watcherExclude patterns ***
            }
        },
        "extensions": {
            "recommendations": [
                "ms-python.python"
            ]
        },
        <*** more other config options ***>
    }

In summary
    In the "folders" value add any SeqWeb decls that are missing
    In the "settings" value
        In the "files.exclude" value add 3 /data subfolder patterns if missing
        In the "search.exclude" and "files.watcherExclude" values also add those, plus the two data repos, if missing
        And preserve any other "settings" as-is
    And preserve any other entries as-is

"""
generate_folders_from_seqweb_config()
INCLUDED_WORKSPACE_DIRS_ = {}    # include SeqWeb dev home
EXCLUDED_SEQWEB_CODE_DATA_DIRS = # these are subfolders in the `seqwebcode/data` tree
    {
        "**/data/output": true,
        "**/data/intermediate": true,
        "**/data/logs": true
    }
EXCLUDED_SEQWEB_DATA_REPO_DIRS = # these could be anywhere in the filesystem
    {
        "**/seqwebdata": true,
        "**/oeisdata": true
    }



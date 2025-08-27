# functions for transforming Cursor/VS Code workspace configs to help support development.

"""
Cursor configs are embodied as dicts; this file implements the SeqWeb-specific dict->dict transforms.
Reading and writing the actual data is implemented by the caller.

The transforms "respectfully" augment an existing "base" dict, never overriding explicit settings.
The augmentation items here are all derived from repo path info obtained from the seqvar store.

A high-level top-down description would be:

The output `augmented_config` is the input `base_config`, overlaid with some augmented entries:

    {**base_config, **augment_to_base}

The `augment_to_base` is an overlay containing augmented "folders" and "settings" entries:

    {"folders": augmented_folders, "settings": augmented_settings}.

The value of `augmented_folders` is computed via a process which is the "moral equivalent" of

    "{**default_folders, **base_config["folders"]}"

However the Cursor "folders" config is not a single dict, but a *list* of dicts that describe the workspace folders.
For SeqWeb development, we merely ensure that for each of our "repo seqvars" there's a folder with a matching "name",
plus a "seqwebdev" folder for the SeqWeb dev home.  We add any of these that are missing, maintianing other content.


The value of `augmented_settings` is given by

    {**base_settings, **augment_to_settings}

where `base_settings` is base_config["settings"].


The `augment_to_settings` are a dict with keys that suppress Cursor indexing on data folders:
    {
        "files.exclude": {**dev_files_exclude, **base_settings`["files.exclude"]}
        "search.exclude": {**data_repos_exclude, **dev_files_exclude, **base_settings["search.exclude"]}
        "files.watcherExclude": {**data_repos_exclude, **dev_files_exclude, **base_settings["files.watcherExclude"]}
    }

These "exclude" settings are dicts whose keys are the excluded path patterns, always with a value of `True`
    `dev_files_exclude` currently simply specifies the `seqwebcode` repo's `data/` folder.
    `data_repos_exclude` gives the paths to the `oeisdata` and `seqwebdata` repos obtained from the seqvar store.

"""

from lib.seqvar.seqvar import get_dict


def get_repo_folders() -> dict[str, str]:
    """
    Returns: repo folder dict obtained from the seqvar store with the "repos." prefix removed from all keys.
    """
    repos_dict = get_dict("repos.*")
    return {key[6:]: value for key, value in repos_dict.items()}


def augmented_config(base_config: dict) -> dict:
    return {**base_config, **augment_to_base(base_config)}


def augment_to_base(base_config: dict) -> dict:
    repo_folders = get_repo_folders()
    return {
        "folders": augmented_folders(base_config, repo_folders),
        "settings": augmented_settings(base_config, repo_folders)
    }


def augmented_folders(base_config: dict, repo_folders: dict) -> list:
    # initialize name:path dict of proposed folders based on seqvar store with the seqwebdev folder
    from lib.seqvar.seqvar import get as get_seqvar
    proposed_folders = {"seqwebdev": get_seqvar("seqwebdev.home")}.copy()

    # then add all the repos to the proposals
    for repo_name, repo_path in repo_folders.items():
        proposed_folders[repo_name] = repo_path

    # collect all the configured folders into result, removing any colliding proposals as we go
    folder_configs = base_config.get("folders", [])
    result = []
    for folder_config in folder_configs:
        # collect configured folder into result
        result.append(folder_config)
        # remove from proposals
        folder_name = folder_config["name"]
        proposed_folders[folder_name] = ""   # empty string is seqvar equivalent of "no value"

    # now add all proposals with non-empty paths
    for proposal_name, proposal_path in proposed_folders.items():
        if proposal_path != "":
            result.append({
                "name": proposal_name,
                "path": proposal_path
            })

    return result


def augmented_settings(base_config: dict, default_folders: dict) -> dict:
    return {**base_config.get("settings", {}), **augment_to_settings(base_config, default_folders)}


def augment_to_settings(base_config: dict, default_folders: dict) -> dict:
    base_settings = base_config.get("settings", {})
    dev_exclude = dev_files_exclude(default_folders)
    data_exclude = data_repos_exclude(default_folders)
    return {
        "files.exclude": {
            **dev_exclude,
            **base_settings.get("files.exclude", {})
        },
        "search.exclude": {
            **data_exclude,
            **dev_exclude,
            **base_settings.get("search.exclude", {})
        },
        "files.watcherExclude": {
            **data_exclude,
            **dev_exclude,
            **base_settings.get("files.watcherExclude", {})
        }
    }


def dev_files_exclude(default_folders: dict) -> dict:
    seqwebcode_path = default_folders["seqwebcode"]
    return {f"{seqwebcode_path}/data/**": True}


def data_repos_exclude(default_folders: dict) -> dict:
    oeisdata_path = default_folders["oeisdata"]
    seqwebdata_path = default_folders["seqwebdata"]
    return {f"{oeisdata_path}/**": True, f"{seqwebdata_path}/**": True}

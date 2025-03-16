# This script copies the default profile to all non-default profiles.
# This is useful for synchronizing settings, bookmarks, and other data
# across all profiles in a Firefox installation.
#
# Note if you receive the following error: 
# shutil.Error: [('/home/account/snap/firefox/common/.mozilla/firefox/n624y6ue.default/lock', '/home/account/snap/firefox/common/.mozilla/firefox/7jdlhbv6.other/lock', "[Errno 2] No such file or directory: '/home/account/snap/firefox/common/.mozilla/firefox/n624y6ue.default/lock'")]
# 
# cd ~/snap/firefox/common/.mozilla/firefox
# rm n624y6ue.default/lock

import os
import shutil

def copy_default_profile_to_non_default(profiles_dir):
    # Get a list of all directories in the profiles directory
    dirs = [d for d in os.listdir(profiles_dir) if os.path.isdir(os.path.join(profiles_dir, d))]

    # Find the default profile directory
    default_profile = None
    for dir in dirs:
        if '.default' in dir:
            default_profile = dir
            break

    if default_profile is None:
        print("Default profile not found.")
        return

    default_path = os.path.join(profiles_dir, default_profile)

    # Define system directories to skip
    system_dirs = ["Crash Reports", "Pending Pings"]

    # Iterate over non-default profile directories
    for dir in dirs:
        if '.default' not in dir and dir not in system_dirs:
            print(f"Copying default profile to {dir}...")
            non_default_path = os.path.join(profiles_dir, dir)
            
            # Clear out the contents of the non-default directory
            shutil.rmtree(non_default_path)
                        
            # Copy the contents of the default profile to the non-default directory
            shutil.copytree(default_path, non_default_path)
            # exit()

    print("Profile synchronization completed.")

# Specify the path to your Firefox profiles directory
profiles_directory = os.path.expanduser("~/snap/firefox/common/.mozilla/firefox")

# Call the function to copy the default profile to non-default profiles
copy_default_profile_to_non_default(profiles_directory)

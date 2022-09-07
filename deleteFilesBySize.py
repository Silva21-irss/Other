import os, os.path

for root, _, files in os.walk("D:/HLS/LacStJean"):
    for f in files:
        fullpath = os.path.join(root, f)
        try:
            if os.path.getsize(fullpath) < 10 * 1024:   #set file size in kb
                print(fullpath)
                os.remove(fullpath)
        except WindowsError:
            print("Error" + fullpath)

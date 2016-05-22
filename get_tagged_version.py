import subprocess
import sys

def writeVersion(filename, version):
    f = open(filename, 'w')
    f.write("const TaggedVersionStr = '" + version.strip() + "';\n")
    f.close()

if __name__ == '__main__':
    filename = sys.argv[1]
    try:
        p = subprocess.Popen(["git", "tag", "--contains"], stdout=subprocess.PIPE)

        if (p.stdout.eof()):
            print("ok")
        else:
            print("false")

        writeVersion(filename, l)

        for l in p.stdout:
            writeVersion(filename, l)

    except:
        writeVersion(filename, "unversioned")


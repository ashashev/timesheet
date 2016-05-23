import subprocess
import sys

def writeVersion(filename, version):
    f = open(filename, 'w')
    f.write("const TaggedVersionStr = '" + version.strip() + "';\n")
    f.close()

if __name__ == '__main__':
    filename = sys.argv[1]
    unknownVersion="0.0.0-#"
    try:
        p = subprocess.Popen(["git", "describe", "--match", "v.*"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        stdoutdata, stderrdata = p.communicate()

        writeVersion(filename, unknownVersion)

        if p.returncode == 0:
            writeVersion(filename, stdoutdata)
        else:
            print(stderrdata)

    except Exception as inst:
        print("error")
        print(type(inst))     # the exception instance
        print(inst.args)      # arguments stored in .args
        print(inst)           # __str__ allows args to be printed directly
        writeVersion(filename, unknownVersion)


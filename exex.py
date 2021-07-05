# # assing output file name
# # out_file_name="ttt.txt"
# out_file_name= sys.argv[2]
#
# out_file_path="./res/"
#
# out_file_full_path_name = out_file_path + out_file_name
#
#
# files=[i for i in os.listdir() if kind in i ]
#
# output=open(out_file_full_path_name, "w")

# file=files[0]

import sys
# filePath='./final_report-kenko(和歌山県)/table/'
filePath='./'

InputFileName=sys.argv[1]
# InputFileName="table_commom_d.tex"
InputFileName=filePath+InputFileName

OutputFineName=sys.argv[2]
# OutputFineName="sss.txt"
OutputFineName=filePath+OutputFineName

import re

inputFile = open(InputFileName)
outputFile=open(OutputFineName, "w")


for i in inputFile:
    i = i.rstrip()

    if re.search(r'\\begin\{tabular\}', i):

        i = re.sub(
        r'\\begin\{tabular\}',
        r'\n\\begin{adjustbox}{width=.5\\textwidth}\n\\begin{tabular}',
        i)

    if re.search(r'\\end\{tabular\}', i):

        i = re.sub(
        r'\\end\{tabular\}',
        r'\\end{tabular}\n\\end{adjustbox}\n',
        i)
    #
    # out = re.sub(
    #
    #
    # i)
    print(i, file=outputFile)

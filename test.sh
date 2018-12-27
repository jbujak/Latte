#! /bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;36m'
BOLD='\e[1m'
RESET='\033[0m'

EXE=latc_x86_64
BAD=test/bad
GOOD=test/good

echo ""
echo -e ${BLUE}${BOLD}Good programs tests${RESET}
total=0
failed=0
for file in $(ls $GOOD/*.lat)
do
	total=$((total+1))
	./$EXE $file >/dev/null 2>&1
	if [ -f $file.in ]
	then
		./out.exe <$file.in >out 2>err
	else
		./out.exe >out 2>err
	fi
	code=$?
	if diff $file.out out 2>&1 >/dev/null
	then
		echo -e $file ${GREEN}OK${RESET}
	else
		echo -e $file ${RED}ERROR${RESET}
		failed=$((failed+1))

	while read line
	do
		echo -n -e '\t'
		echo $line
	done <out
	fi
	rm out.exe
	rm out.o
	rm out.s
	rm err
done

echo ""
echo -e ${BLUE}${BOLD}Bad programs tests${RESET}
for file in $(ls $BAD/*.lat)
do
	total=$((total+1))
	./$EXE $file >out 2>err
	code=$?
	expected_code=255
	if [ $code = $expected_code ]
	then
		echo -n -e $file ${GREEN}OK${RESET}
	echo -n -e '\n\t'
		cat err
	else
		echo -e $file ${RED}ERROR: $code${RESET}
		failed=$((failed+1))
	fi
	rm out
	rm err
done

echo ""
echo -e Total number of tests: $total
if [ $failed == 0 ]
then echo -e ${GREEN}All tests passed${RESET}
else echo -e ${RED}Failed tests: $failed${RESET}
fi

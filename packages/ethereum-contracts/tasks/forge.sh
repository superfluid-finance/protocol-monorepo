counter="0"
while true; do
    yarn run-foundry test --hardhat --no-match-contract Fork -vv
    if [[ $? != 0 ]]
    then
        echo "Failed on test run #$counter"
        break
    fi
    counter=$(( $counter + 1 ))
done
#!/bin/sh
D="echo \"#!/bin/sh\"\necho \"D=\"\`echo -E \$D\`\"\necho -e \"\$D\"\n"
echo "#!/bin/sh"
echo "D=\"`echo -E $D`\"" 
echo -e "$D"

# still not working :(

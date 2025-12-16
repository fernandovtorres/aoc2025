import * as fs from 'fs';

try {
    const fileContent = fs.readFileSync('../inputs/day3.txt', 'utf-8');
    const lines = fileContent.split(/r?\n/);
    let sum = 0n;
    for (const line of lines) {
        if (line.length == 0) continue; 
        const stack = [];
        const size = 12;
        for (let i = 0; i < line.length; i++) {
            const n = line[i] - '0';

            while (stack.length > 0 && stack[stack.length - 1] < n &&
                (stack.length - 1) + (line.length - i) >= size) {
                stack.pop();
            };

            if (stack.length < size) {
                stack.push(n);
            };
        };
        sum += BigInt(stack.join(''));
    };
    console.log(sum);
} catch (error) {
    console.error(error);
}


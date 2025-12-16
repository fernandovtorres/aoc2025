import * as fs from 'fs';



try {
    const fileContent = fs.readFileSync('../inputs/day3.txt', 'utf-8');
    const lines = fileContent.split(/r?\n/);
    let sum = 0;
    let maxNum = -1;
    let secNum = -1;
    for (const line of lines) {
        if (line.length == 0) continue;
        for (let i = 0; i < line.length; i++) {
            if (line[i] - '0' > secNum) {
                secNum = line[i] - '0';
            };
            if (i != line.length-1 && line[i] - '0' > maxNum) {
                maxNum = line[i] - '0';
                secNum = -1;
            };
        };
        sum += (maxNum * 10) + secNum;
        maxNum = -1;
        secNum = -1;
    };
    console.log(sum);
} catch (error) {
    console.error(error);
}


#include <bits/stdc++.h>
using namespace std;

#define p pair<long long, long long>

vector<p> parse() {
    string line;
    long long a, b;
    ifstream f("../inputs/day2.txt");

    vector<p> d;
    
    while (getline(f, line, ',')) {
        sscanf(line.c_str(), "%lld-%lld", &a, &b); 
        p c = make_pair(a, b);
        d.push_back(c);
    }

    return d;
}


int main(void) {
    vector<p> lista = parse();
    long long res = 0;
    for (auto x : lista) {
        for (long long i = x.first; i <= x.second; i++) {
            if (i <= 10) continue;

            int dig = 0;
            long long t = i;
            while (t > 0) {
                t /= 10;
                dig++;
            }

            for (int j = 1; j <= dig /2 ; j++) {
                if (dig % j == 0) {
                    int d = dig / j;

                    long long mask = 0;
                    long long block = 1;

                    for (int a = 0; a < j; a++) block *= 10;

                    long long curr = 1;
                    for (int k = 0; k < d; k++) {
                        mask += curr;
                        if (k < d - 1) curr *= block;
                    }

                    if (i % mask == 0) {
                        res += i;
                        break;
                    }
                }
            }

        }
    }
    cout << res << endl;
    return 0;
}

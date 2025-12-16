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
        string first = to_string(x.first);
        string second = to_string(x.second);

        for (int i = first.length(); i <= second.length(); i++) {
            if (i & 1) continue;

            int metade = i / 2;
            long long p10 = pow(10, metade);

            long long min_r = x.first / (p10 + 1);
            long long max_r = x.second / (p10 + 1);

            for (long long r = min_r; r <= max_r + 1; r++) {
                long long invalid = r * (p10 + 1);

                if (invalid >= x.first && invalid <= x.second) {
                    if (to_string(r).length() == metade) {
                        res += invalid;
                    }
                }
            }
        }
    }
    cout << res << endl;
    return 0;
}

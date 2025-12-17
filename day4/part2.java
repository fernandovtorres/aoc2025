import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;

public class part2 {

    private static List<String> parse() {
        String file = "../inputs/day4.txt";
        List<String> matriz = new ArrayList<String>();

        try (BufferedReader reader = Files.newBufferedReader(Paths.get(file), StandardCharsets.UTF_8)) {
            String linha;
            while((linha = reader.readLine()) != null) {
                matriz.add(linha);
            }
            matriz.add(0, ".".repeat(matriz.get(0).length()));
            matriz.add(".".repeat(matriz.get(0).length()));

            for (int i = 0; i < matriz.size(); i++) {
                matriz.set(i, '.' + matriz.get(i) + '.');
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        return matriz;

    }
    public static void main(String[] args) {
        List<String> matriz = parse();
        char alvo = '@';
        int linhas = matriz.size();
        int colunas = matriz.get(0).length();

        char[][] grid = new char[linhas][colunas];
        int[][] viz = new int[linhas][colunas];
        Queue<int[]> fila = new ArrayDeque<int[]>();

        int res = 0;

        for (int i = 0; i < linhas; i++) {
            grid[i] = matriz.get(i).toCharArray();
        }

        for (int r = 1; r < linhas - 1; r++) {
            for (int c = 1; c < colunas - 1; c++) {
                if (grid[r][c] == alvo) {
                    int count = 0;
                    for (int i = r - 1; i <= r + 1; i++) {
                        for (int j = c - 1; j <= c + 1; j++) {
                            if (i == r && j == c) continue;
                            if (grid[i][j] == '@') {
                                count++;
                            }
                        }
                    }
                    viz[r][c] = count;
                    
                    if (count < 4) {
                        fila.add(new int[]{r, c});
                    }
                }
            }
        }

        int[] dLin = {-1, -1, -1,  0,  0,  1,  1,  1};
        int[] dCol = {-1,  0,  1, -1,  1, -1,  0,  1};

        while (!fila.isEmpty()) {
            int[] curr = fila.poll();
            int r = curr[0];
            int c = curr[1];

            if (grid[r][c] != alvo) continue;

            grid[r][c] = 'x';
            res ++;
            
            for (int i = 0; i < 8; i++) {
                int dr = r + dLin[i];
                int dc = c + dCol[i];

                if (grid[dr][dc] == alvo) {
                    viz[dr][dc]--;

                    if (viz[dr][dc] < 4) {
                        fila.add(new int[]{dr, dc});
                    }
                }
            }
        }
        System.out.println(res);
        

    }

}

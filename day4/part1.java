import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class part1 {

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
        int res = 0;
        for (int i = 1; i < matriz.size()-1; i++) {
            String actLin = matriz.get(i);

            for (int j = 1; j < actLin.length()-1; j++) {
                if (matriz.get(i).charAt(j) == '.') continue;
                int act = 0;                
                act += matriz.get(i-1).charAt(j-1) == alvo ? 1 : 0;
                act += matriz.get(i-1).charAt(j) == alvo ? 1 : 0;
                act += matriz.get(i-1).charAt(j+1) == alvo ? 1 : 0;
                act += matriz.get(i).charAt(j-1) == alvo ? 1 : 0;
                act += matriz.get(i).charAt(j+1) == alvo ? 1 : 0;
                act += matriz.get(i+1).charAt(j-1) == alvo ? 1 : 0;
                act += matriz.get(i+1).charAt(j) == alvo ? 1 : 0;
                act += matriz.get(i+1).charAt(j+1) == alvo ? 1 : 0;
                if (act < 4) res += 1;
            }
        }
        System.out.println(res);

    }

}

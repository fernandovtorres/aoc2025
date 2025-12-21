using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    struct Point
    {
        public int Id;
        public long X, Y, Z;
    }

    struct Edge
    {
        public int U;
        public int V;
        public long DistSq;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("../inputs/day8.txt");
        var points = new List<Point>();

        for (int i = 0; i < lines.Length; i++)
        {
            var parts = lines[i].Split(',');
            points.Add(new Point
            {
                Id = i,
                X = long.Parse(parts[0]),
                Y = long.Parse(parts[1]),
                Z = long.Parse(parts[2])
            });
        }

        var edges = new List<Edge>();
        for (int i = 0; i < points.Count; i++)
        {
            for (int j = i + 1; j < points.Count; j++)
            {
                long dx = points[i].X - points[j].X;
                long dy = points[i].Y - points[j].Y;
                long dz = points[i].Z - points[j].Z;
                long distSq = dx * dx + dy * dy + dz * dz;

                edges.Add(new Edge { U = i, V = j, DistSq = distSq });
            }
        }

        edges.Sort((a, b) => a.DistSq.CompareTo(b.DistSq));

        int[] parent = new int[points.Count];
        int[] size = new int[points.Count];

        for (int i = 0; i < points.Count; i++)
        {
            parent[i] = i;
            size[i] = 1;
        }

        int Find(int node)
        {
            if (parent[node] != node)
                parent[node] = Find(parent[node]);
            return parent[node];
        }

        void Union(int u, int v)
        {
            int rootU = Find(u);
            int rootV = Find(v);

            if (rootU != rootV)
            {
                if (size[rootU] < size[rootV])
                {
                    parent[rootU] = rootV;
                    size[rootV] += size[rootU];
                }
                else
                {
                    parent[rootV] = rootU;
                    size[rootU] += size[rootV];
                }
            }
        }

        for (int i = 0; i < edges.Count; i++)
        {
            
            Union(edges[i].U, edges[i].V);
        }

        var clusterSizes = new List<long>();
        var processedRoots = new HashSet<int>();

        for (int i = 0; i < points.Count; i++)
        {
            int root = Find(i);
            if (processedRoots.Add(root))
            {
                clusterSizes.Add(size[root]);
            }
        }

        var top3 = clusterSizes.OrderByDescending(x => x).Take(3).ToList();
        long result = top3[0] * top3[1] * top3[2];

        Console.WriteLine(result);
    }
}

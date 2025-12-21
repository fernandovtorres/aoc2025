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
        public long XMult;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("../../inputs/day8.txt");
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
                long xMult = points[i].X * points[j].X;
                long distSq = dx * dx + dy * dy + dz * dz;

                edges.Add(new Edge { U = i, V = j, DistSq = distSq, XMult = xMult });
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

        bool Union(int u, int v)
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
                return true;
            }
            return false;
        }

        Edge last_edge = new Edge{ U = -1, V = -1, DistSq = 0, XMult = 0};

        for (int i = 0; i < edges.Count; i++)
        {
            if(Union(edges[i].U, edges[i].V)) last_edge = edges[i];
        }

        long result = last_edge.XMult;

        Console.WriteLine(result);
    }
}

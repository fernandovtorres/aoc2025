using Random
using Base.Threads

struct Point
    r::Int
    c::Int
end

struct Shape
    id::Int
    cells::Vector{Point}
    w::Int
    h::Int
    area::Int
end

function parse_input(content::String)
    cleaned = replace(content, r"\""=>"")
    lines = split(cleaned, '\n')

    shapes = Dict{Int, Vector{Shape}}()
    queries = []

    s0_cells = [Point(0,0), Point(0,2), Point(1,0), Point(1,1), Point(1,2), Point(2,0), Point(2,2)]
    s1_cells = [Point(0,0), Point(0,1), Point(0,2), Point(1,1), Point(1,2), Point(2,2)]
    s2_cells = [Point(0,1), Point(0,2), Point(1,0), Point(1,1), Point(2,0)]
    s3_cells = [Point(0,0), Point(0,1), Point(0,2), Point(1,2), Point(2,0), Point(2,1), Point(2,2)]
    s4_cells = [Point(0,1), Point(0,2), Point(1,0), Point(1,1), Point(1,2), Point(2,0), Point(2,2)]
    s5_cells = [Point(0,0), Point(0,1), Point(0,2), Point(1,0), Point(1,1), Point(1,2), Point(2,0)]

    base_shapes = [s0_cells, s1_cells, s2_cells, s3_cells, s4_cells, s5_cells]

    for (id, cells) in enumerate(base_shapes)
        real_id = id - 1
        variations = generate_variations(real_id, cells)
        shapes[real_id] = variations
    end

    token_stream = String[]
    for line in lines
        if isempty(strip(line)); continue; end
        if occursin("#", line); continue; end

        parts = split(replace(line, ":" => " "))
        append!(token_stream, parts)
    end

    i = 1
    while i <= length(token_stream)
        token = token_stream[i]

        m = match(r"^(\d+)x(\d+)$", token)
        if m !== nothing
            w = parse(Int, m.captures[1])
            h = parse(Int, m.captures[2])

            counts = Int[]
            valid_query = true
            for j in 1:6
                if i + j > length(token_stream)
                    valid_query = false
                    break
                end
                push!(counts, parse(Int, token_stream[i+j]))
            end

            if valid_query
                push!(queries, (w, h, counts))
                i += 7
            else
                break
            end
        else
            i += 1
        end
    end

    return shapes, queries
end

function generate_variations(id::Int, cells::Vector{Point})
    variations = Shape[]
    seen = Set{Set{Point}}()
    current_cells = deepcopy(cells)

    for flip in 0:1
        for rot in 0:3
            min_r = minimum(p.r for p in current_cells)
            min_c = minimum(p.c for p in current_cells)
            max_r = maximum(p.r for p in current_cells)
            max_c = maximum(p.c for p in current_cells)

            norm_cells = [Point(p.r - min_r, p.c - min_c) for p in current_cells]
            cell_set = Set(norm_cells)

            if !(cell_set in seen)
                push!(seen, cell_set)
                w = (max_c - min_c) + 1
                h = (max_r - min_r) + 1
                push!(variations, Shape(id, norm_cells, w, h, length(norm_cells)))
            end

            current_cells = [Point(p.c, -p.r) for p in current_cells]
        end
        current_cells = [Point(p.r, -p.c) for p in cells]
    end

    return variations
end

function solve_region(w::Int, h::Int, counts::Vector{Int}, shapes::Dict{Int, Vector{Shape}})
    items = Tuple{Int, Int}[]
    total_area = 0

    for (sid, count) in enumerate(counts)
        id = sid - 1
        shape_area = shapes[id][1].area
        total_area += shape_area * count
        for _ in 1:count
            push!(items, (id, length(items)+1))
        end
    end

    if total_area > (w * h)
        return false
    end

    n_items = length(items)
    item_configs = Vector{Tuple{Int, Int, Int}}(undef, n_items)
    grid = zeros(Int16, h, w)

    function apply_shape!(g, var_idx, r, c, delta, shape_list)
        s = shape_list[var_idx]
        for cell in s.cells
            rr, cc = r + cell.r, c + cell.c
            if checkbounds(Bool, g, rr, cc)
                @inbounds g[rr, cc] += delta
            end
        end
    end

    function in_bounds(var_idx, r, c, shape_list, grid_h, grid_w)
        s = shape_list[var_idx]
        return r >= 1 && c >= 1 && (r + s.h - 1) <= grid_h && (c + s.w - 1) <= grid_w
    end

    for i in 1:n_items
        sid = items[i][1]
        vars = shapes[sid]

        v_idx = rand(1:length(vars))
        s = vars[v_idx]

        max_r = h - s.h + 1
        max_c = w - s.w + 1

        if max_r < 1 || max_c < 1
            return false
        end

        rr = rand(1:max_r)
        cc = rand(1:max_c)

        item_configs[i] = (v_idx, rr, cc)
        apply_shape!(grid, v_idx, rr, cc, 1, vars)
    end

    max_iters = 100000
    iter = 0
    while iter < max_iters
        conflicts = Int[]
        has_overlaps = false

        for i in 1:n_items
            (v, r, c) = item_configs[i]
            sid = items[i][1]
            vars = shapes[sid]
            s = vars[v]

            is_conflicting = false
            for cell in s.cells
                if grid[r + cell.r, c + cell.c] > 1
                    is_conflicting = true
                    has_overlaps = true
                    break
                end
            end

            if is_conflicting
                push!(conflicts, i)
            end
        end

        if !has_overlaps
            return true
        end

        item_idx = rand(conflicts)
        sid = items[item_idx][1]
        vars = shapes[sid]

        (old_v, old_r, old_c) = item_configs[item_idx]
        apply_shape!(grid, old_v, old_r, old_c, -1, vars)

        best_score = 999999
        best_configs = Tuple{Int, Int, Int}[]
        candidates_to_try = 50

        for _ in 1:candidates_to_try
            v = rand(1:length(vars))
            s = vars[v]

            mr = h - s.h + 1
            mc = w - s.w + 1
            if mr < 1 || mc < 1; continue; end

            rr = rand(1:mr)
            cc = rand(1:mc)

            cost = 0
            for cell in s.cells
                if grid[rr + cell.r, cc + cell.c] > 0
                    cost += grid[rr + cell.r, cc + cell.c]
                end
            end

            if cost < best_score
                best_score = cost
                empty!(best_configs)
                push!(best_configs, (v, rr, cc))
            elseif cost == best_score
                push!(best_configs, (v, rr, cc))
            end

            if cost == 0; break; end
        end

        (new_v, new_r, new_c) = rand(best_configs)

        item_configs[item_idx] = (new_v, new_r, new_c)
        apply_shape!(grid, new_v, new_r, new_c, 1, vars)

        iter += 1
    end

    return false
end

function main()
    content = read("../inputs/day12.txt", String)

    shapes, queries = parse_input(content)


    valid_count = Atomic{Int}(0)

    @threads for (w, h, counts) in queries
        if solve_region(w, h, counts, shapes)
            atomic_add!(valid_count, 1)
        end
    end

    println(valid_count[])
end

main()

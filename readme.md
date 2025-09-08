# Eidolon(ia) language

Eidolon is a simple, yet powerful programming language designed for ease of use and performance. It is a dynamically typed language that supports a wide range of features, including functions, variables, and control structures. I did it for fun and to use it in my projects. 

## Syntax

First function tests
```eidolon
fun distance[x1, y1, x2, y2] = ((x2 - x1)^2 + (y2 - y1)^2)^0.5

distance[$p_x, $p_y, $d_x, $d_y]
```

Or homming missile:

```eidolon
fun normalize[v] = {
  let len = length[v]
  if len > 0 {
    v / len
  } else {
    v
  }
}

let vector_to_player = $player_pos - $missile_pos
let desired_direction = normalize[vector_to_player]
let target_velocity = desired_direction * $missile_speed
let turn_factor = clamp[$turn_rate * $delta_time, 0, 1]
let new_velocity = lerp[$missile_velocity, target_velocity, turn_factor]

$missile_pos + new_velocity * $delta_time
```

You can see more examples in `examples` directory.

## TODO
- [ ] Add more examples
- [ ] Implement plugin for JetBrains IDE and Visual Studio Code
- [x] Implement JNI bridge — https://github.com/Weever1337/nExus-bridge (Java 8+)
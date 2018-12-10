module Input where

data Dir = F | B | L | R

data Event = GO Dir | QUIT

type Events = [Event]

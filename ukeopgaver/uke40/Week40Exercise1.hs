module Week40Exercise1 where

data PlanetarySystem = PlanetarySystem
  { sunName :: String,
    planets :: [Planet]
  }
  deriving (Eq, Show)

data Planet = Planet
  { planetName :: String,
    moons :: [Moon]
  }
  deriving (Eq, Show)

newtype Moon = Moon {moonName :: String}
  deriving (Eq, Show)


numberOfMoons :: PlanetarySystem -> Integer
numberOfMoons PlanetarySystem {sunName=_, planets=planets} = sum $ map numberOfMoonsOnPlanet planets

numberOfMoonsOnPlanet :: Planet -> Integer
numberOfMoonsOnPlanet Planet {planetName=_, moons=moons} = fromIntegral (length moons)

atLeastOneMoon :: PlanetarySystem -> [Planet]
atLeastOneMoon PlanetarySystem {sunName=_, planets=planets} = filter hasMoons planets

hasMoons :: Planet -> Bool
hasMoons Planet {planetName=_, moons=moons} = not (null moons)

solarSystem :: PlanetarySystem
solarSystem =
  PlanetarySystem
    { sunName = "The Sun",
      planets =
        [ Planet
            { planetName = "Mercury",
              moons = []
            },
          Planet
            { planetName = "Venus",
              moons = []
            },
          Planet
            { planetName = "Earth",
              moons =
                [ Moon {moonName = "The Moon"}
                ]
            },
          Planet
            { planetName = "Mars",
              moons =
                [ Moon {moonName = "Deimos"},
                  Moon {moonName = "Phobos"}
                ]
            },
          Planet
            { planetName = "Jupiter",
              moons =
                [ Moon {moonName = "Io"},
                  Moon {moonName = "Europa"},
                  Moon {moonName = "Ganymede"},
                  Moon {moonName = "Callisto"}
                ]
            },
          Planet
            { planetName = "Saturn",
              moons =
                [ Moon {moonName = "Mimas"},
                  Moon {moonName = "Enceladus"},
                  Moon {moonName = "Tethys"},
                  Moon {moonName = "Dione"},
                  Moon {moonName = "Rhea"},
                  Moon {moonName = "Titan"},
                  Moon {moonName = "Hyperion"},
                  Moon {moonName = "Iapetus"},
                  Moon {moonName = "Phoebe"}
                ]
            },
          Planet
            { planetName = "Uranus",
              moons =
                [ Moon {moonName = "Miranda"},
                  Moon {moonName = "Ariel"},
                  Moon {moonName = "Umbriel"},
                  Moon {moonName = "Titania"},
                  Moon {moonName = "Oberon"}
                ]
            },
          Planet
            { planetName = "Neptune",
              moons =
                [ Moon {moonName = "Proteus"},
                  Moon {moonName = "Triton"},
                  Moon {moonName = "Nereid"}
                ]
            }
        ]
    }
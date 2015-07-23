-module(bk_animals).
-compile(export_all).

has_covering(dog,hair)->true;
has_covering(dolphin,none)->true;
has_covering(platypus,hair)->true;
has_covering(bat,hair)->true;
has_covering(trout,scales)->true;
has_covering(herring,scales)->true;
has_covering(shark,none)->true;
has_covering(eel,none)->true;
has_covering(lizard,scales)->true;
has_covering(crocodile,scales)->true;
has_covering(t_rex,scales)->true;
has_covering(snake,scales)->true;
has_covering(turtle,scales)->true;
has_covering(eagle,feathers)->true;
has_covering(ostrich,feathers)->true;
has_covering(penguin,feathers)->true.

has_legs(dog,4)->true;
has_legs(dolphin,0)->true;
has_legs(platypus,2)->true;
has_legs(bat,2)->true;
has_legs(trout,0)->true;
has_legs(herring,0)->true;
has_legs(shark,0)->true;
has_legs(eel,0)->true;
has_legs(lizard,4)->true;
has_legs(crocodile,4)->true;
has_legs(t_rex,4)->true;
has_legs(snake,0)->true;
has_legs(turtle,4)->true;
has_legs(eagle,2)->true;
has_legs(ostrich,2)->true;
has_legs(penguin,2)->true.

has_milk(dog)->true;
has_milk(dolphin)->true;
has_milk(bat)->true;
has_milk(platypus)->true;
has_milk(cat)->true.

homeothermic(dog)->true;
homeothermic(dolphin)->true;
homeothermic(platypus)->true;
homeothermic(bat)->true;
homeothermic(eagle)->true;
homeothermic(ostrich)->true;
homeothermic(cat)->true;
homeothermic(penguin)->true.


habitat(dog,land)->true;
habitat(dolphin,water)->true;
habitat(platypus,water)->true;
habitat(bat,air)->true;
habitat(bat,caves)->true;
habitat(trout,water)->true;
habitat(herring,water)->true;
habitat(shark,water)->true;
habitat(eel,water)->true;
habitat(lizard,land)->true;
habitat(crocodile,water)->true;
habitat(crocodile,land)->true;
habitat(t_rex,land)->true;
habitat(snake,land)->true;
habitat(turtle,water)->true;
habitat(eagle,air)->true;
habitat(eagle,land)->true;
habitat(ostrich,land)->true;
habitat(penguin,water)->true.

has_eggs(platypus)->true;
has_eggs(trout)->true;
has_eggs(herring)->true;
has_eggs(shark)->true;
has_eggs(eel)->true;
has_eggs(lizard)->true;
has_eggs(crocodile)->true;
has_eggs(t_rex)->true;
has_eggs(snake)->true;
has_eggs(turtle)->true;
has_eggs(eagle)->true;
has_eggs(ostrich)->true;
has_eggs(penguin)->true.

has_gills(trout)->true;
has_gills(herring)->true;
has_gills(shark)->true;
has_gills(eel)->true.

animal(cat)->true; 
animal(dragon)->true;
animal(girl)->true;
animal(boy)->true.



#Neel Das v1 01.29.2021
#The approach is to generate word guesses based on constructing syllables
#Syllables are composed of three parts:
#	A mandatory nucleus
#	An optional onset before the nucleus
#	An optional coda after the nucleus
#	The entire word may also contain a suffix
# For more information, refer to https://en.wikipedia.org/wiki/English_phonology
#
# An alternative approach is to iterate through the dictionary and check if the word satisfies the requirements
# Indeed, this approach is much simpler, and faster. However, each guess for that is O(D) and this is way more interesting
# This is more computationally intense as well. The wordle dictionary is ~5000 words. The english dictionary contains 171,000 words
# 50 onsets * 50 nuclei, * 100 codas * 2 suffix ~500,000 combinations for 1 syllable.
# This is a fun exercise though, and it is a more human approach to solving the problem and might be able to be better optimized

wordleDict = ["cigar", "rebut", "sissy", "humph", "awake", "blush", "focal", "evade", "naval", "serve", "heath", "dwarf", "model", "karma", "stink", "grade", "quiet", "bench", "abate", "feign", "major", "death", "fresh", "crust", "stool", "colon", "abase", "marry", "react", "batty", "pride", "floss", "helix", "croak", "staff", "paper", "unfed", "whelp", "trawl", "outdo", "adobe", "crazy", "sower", "repay", "digit", "crate", "cluck", "spike", "mimic", "pound", "maxim", "linen", "unmet", "flesh", "booby", "forth", "first", "stand", "belly", "ivory", "seedy", "print", "yearn", "drain", "bribe", "stout", "panel", "crass", "flume", "offal", "agree", "error", "swirl", "argue", "bleed", "delta", "flick", "totem", "wooer", "front", "shrub", "parry", "biome", "lapel", "start", "greet", "goner", "golem", "lusty", "loopy", "round", "audit", "lying", "gamma", "labor", "islet", "civic", "forge", "corny", "moult", "basic", "salad", "agate", "spicy", "spray", "essay", "fjord", "spend", "kebab", "guild", "aback", "motor", "alone", "hatch", "hyper", "thumb", "dowry", "ought", "belch", "dutch", "pilot", "tweed", "comet", "jaunt", "enema", "steed", "abyss", "growl", "fling", "dozen", "boozy", "erode", "world", "gouge", "click", "briar", "great", "altar", "pulpy", "blurt", "coast", "duchy", "groin", "fixer", "group", "rogue", "badly", "smart", "pithy", "gaudy", "chill", "heron", "vodka", "finer", "surer", "radio", "rouge", "perch", "retch", "wrote", "clock", "tilde", "store", "prove", "bring", "solve", "cheat", "grime", "exult", "usher", "epoch", "triad", "break", "rhino", "viral", "conic", "masse", "sonic", "vital", "trace", "using", "peach", "champ", "baton", "brake", "pluck", "craze", "gripe", "weary", "picky", "acute", "ferry", "aside", "tapir", "troll", "unify", "rebus", "boost", "truss", "siege", "tiger", "banal", "slump", "crank", "gorge", "query", "drink", "favor", "abbey", "tangy", "panic", "solar", "shire", "proxy", "point", "robot", "prick", "wince", "crimp", "knoll", "sugar", "whack", "mount", "perky", "could", "wrung", "light", "those", "moist", "shard", "pleat", "aloft", "skill", "elder", "frame", "humor", "pause", "ulcer", "ultra", "robin", "cynic", "agora", "aroma", "caulk", "shake", "pupal", "dodge", "swill", "tacit", "other", "thorn", "trove", "bloke", "vivid", "spill", "chant", "choke", "rupee", "nasty", "mourn", "ahead", "brine", "cloth", "hoard", "sweet", "month", "lapse", "watch", "today", "focus", "smelt", "tease", "cater", "movie", "lynch", "saute", "allow", "renew", "their", "slosh", "purge", "chest", "depot", "epoxy", "nymph", "found", "shall", "harry", "stove", "lowly", "snout", "trope", "fewer", "shawl", "natal", "fibre", "comma", "foray", "scare", "stair", "black", "squad", "royal", "chunk", "mince", "slave", "shame", "cheek", "ample", "flair", "foyer", "cargo", "oxide", "plant", "olive", "inert", "askew", "heist", "shown", "zesty", "hasty", "trash", "fella", "larva", "forgo", "story", "hairy", "train", "homer", "badge", "midst", "canny", "fetus", "butch", "farce", "slung", "tipsy", "metal", "yield", "delve", "being", "scour", "glass", "gamer", "scrap", "money", "hinge", "album", "vouch", "asset", "tiara", "crept", "bayou", "atoll", "manor", "creak", "showy", "phase", "froth", "depth", "gloom", "flood", "trait", "girth", "piety", "payer", "goose", "float", "donor", "atone", "primo", "apron", "blown", "cacao", "loser", "input", "gloat", "awful", "brink", "smite", "beady", "rusty", "retro", "droll", "gawky", "hutch", "pinto", "gaily", "egret", "lilac", "sever", "field", "fluff", "hydro", "flack", "agape", "wench", "voice", "stead", "stalk", "berth", "madam", "night", "bland", "liver", "wedge", "augur", "roomy", "wacky", "flock", "angry", "bobby", "trite", "aphid", "tryst", "midge", "power", "elope", "cinch", "motto", "stomp", "upset", "bluff", "cramp", "quart", "coyly", "youth", "rhyme", "buggy", "alien", "smear", "unfit", "patty", "cling", "glean", "label", "hunky", "khaki", "poker", "gruel", "twice", "twang", "shrug", "treat", "unlit", "waste", "merit", "woven", "octal", "needy", "clown", "widow", "irony", "ruder", "gauze", "chief", "onset", "prize", "fungi", "charm", "gully", "inter", "whoop", "taunt", "leery", "class", "theme", "lofty", "tibia", "booze", "alpha", "thyme", "eclat", "doubt", "parer", "chute", "stick", "trice", "alike", "sooth", "recap", "saint", "liege", "glory", "grate", "admit", "brisk", "soggy", "usurp", "scald", "scorn", "leave", "twine", "sting", "bough", "marsh", "sloth", "dandy", "vigor", "howdy", "enjoy", "valid", "ionic", "equal", "unset", "floor", "catch", "spade", "stein", "exist", "quirk", "denim", "grove", "spiel", "mummy", "fault", "foggy", "flout", "carry", "sneak", "libel", "waltz", "aptly", "piney", "inept", "aloud", "photo", "dream", "stale", "vomit", "ombre", "fanny", "unite", "snarl", "baker", "there", "glyph", "pooch", "hippy", "spell", "folly", "louse", "gulch", "vault", "godly", "threw", "fleet", "grave", "inane", "shock", "crave", "spite", "valve", "skimp", "claim", "rainy", "musty", "pique", "daddy", "quasi", "arise", "aging", "valet", "opium", "avert", "stuck", "recut", "mulch", "genre", "plume", "rifle", "count", "incur", "total", "wrest", "mocha", "deter", "study", "lover", "safer", "rivet", "funny", "smoke", "mound", "undue", "sedan", "pagan", "swine", "guile", "gusty", "equip", "tough", "canoe", "chaos", "covet", "human", "udder", "lunch", "blast", "stray", "manga", "melee", "lefty", "quick", "paste", "given", "octet", "risen", "groan", "leaky", "grind", "carve", "loose", "sadly", "spilt", "apple", "slack", "honey", "final", "sheen", "eerie", "minty", "slick", "derby", "wharf", "spelt", "coach", "erupt", "singe", "price", "spawn", "fairy", "jiffy", "filmy", "stack", "chose", "sleep", "ardor", "nanny", "niece", "woozy", "handy", "grace", "ditto", "stank", "cream", "usual", "diode", "valor", "angle", "ninja", "muddy", "chase", "reply", "prone", "spoil", "heart", "shade", "diner", "arson", "onion", "sleet", "dowel", "couch", "palsy", "bowel", "smile", "evoke", "creek", "lance", "eagle", "idiot", "siren", "built", "embed", "award", "dross", "annul", "goody", "frown", "patio", "laden", "humid", "elite", "lymph", "edify", "might", "reset", "visit", "gusto", "purse", "vapor", "crock", "write", "sunny", "loath", "chaff", "slide", "queer", "venom", "stamp", "sorry", "still", "acorn", "aping", "pushy", "tamer", "hater", "mania", "awoke", "brawn", "swift", "exile", "birch", "lucky", "freer", "risky", "ghost", "plier", "lunar", "winch", "snare", "nurse", "house", "borax", "nicer", "lurch", "exalt", "about", "savvy", "toxin", "tunic", "pried", "inlay", "chump", "lanky", "cress", "eater", "elude", "cycle", "kitty", "boule", "moron", "tenet", "place", "lobby", "plush", "vigil", "index", "blink", "clung", "qualm", "croup", "clink", "juicy", "stage", "decay", "nerve", "flier", "shaft", "crook", "clean", "china", "ridge", "vowel", "gnome", "snuck", "icing", "spiny", "rigor", "snail", "flown", "rabid", "prose", "thank", "poppy", "budge", "fiber", "moldy", "dowdy", "kneel", "track", "caddy", "quell", "dumpy", "paler", "swore", "rebar", "scuba", "splat", "flyer", "horny", "mason", "doing", "ozone", "amply", "molar", "ovary", "beset", "queue", "cliff", "magic", "truce", "sport", "fritz", "edict", "twirl", "verse", "llama", "eaten", "range", "whisk", "hovel", "rehab", "macaw", "sigma", "spout", "verve", "sushi", "dying", "fetid", "brain", "buddy", "thump", "scion", "candy", "chord", "basin", "march", "crowd", "arbor", "gayly", "musky", "stain", "dally", "bless", "bravo", "stung", "title", "ruler", "kiosk", "blond", "ennui", "layer", "fluid", "tatty", "score", "cutie", "zebra", "barge", "matey", "bluer", "aider", "shook", "river", "privy", "betel", "frisk", "bongo", "begun", "azure", "weave", "genie", "sound", "glove", "braid", "scope", "wryly", "rover", "assay", "ocean", "bloom", "irate", "later", "woken", "silky", "wreck", "dwelt", "slate", "smack", "solid", "amaze", "hazel", "wrist", "jolly", "globe", "flint", "rouse", "civil", "vista", "relax", "cover", "alive", "beech", "jetty", "bliss", "vocal", "often", "dolly", "eight", "joker", "since", "event", "ensue", "shunt", "diver", "poser", "worst", "sweep", "alley", "creed", "anime", "leafy", "bosom", "dunce", "stare", "pudgy", "waive", "choir", "stood", "spoke", "outgo", "delay", "bilge", "ideal", "clasp", "seize", "hotly", "laugh", "sieve", "block", "meant", "grape", "noose", "hardy", "shied", "drawl", "daisy", "putty", "strut", "burnt", "tulip", "crick", "idyll", "vixen", "furor", "geeky", "cough", "naive", "shoal", "stork", "bathe", "aunty", "check", "prime", "brass", "outer", "furry", "razor", "elect", "evict", "imply", "demur", "quota", "haven", "cavil", "swear", "crump", "dough", "gavel", "wagon", "salon", "nudge", "harem", "pitch", "sworn", "pupil", "excel", "stony", "cabin", "unzip", "queen", "trout", "polyp", "earth", "storm", "until", "taper", "enter", "child", "adopt", "minor", "fatty", "husky", "brave", "filet", "slime", "glint", "tread", "steal", "regal", "guest", "every", "murky", "share", "spore", "hoist", "buxom", "inner", "otter", "dimly", "level", "sumac", "donut", "stilt", "arena", "sheet", "scrub", "fancy", "slimy", "pearl", "silly", "porch", "dingo", "sepia", "amble", "shady", "bread", "friar", "reign", "dairy", "quill", "cross", "brood", "tuber", "shear", "posit", "blank", "villa", "shank", "piggy", "freak", "which", "among", "fecal", "shell", "would", "algae", "large", "rabbi", "agony", "amuse", "bushy", "copse", "swoon", "knife", "pouch", "ascot", "plane", "crown", "urban", "snide", "relay", "abide", "viola", "rajah", "straw", "dilly", "crash", "amass", "third", "trick", "tutor", "woody", "blurb", "grief", "disco", "where", "sassy", "beach", "sauna", "comic", "clued", "creep", "caste", "graze", "snuff", "frock", "gonad", "drunk", "prong", "lurid", "steel", "halve", "buyer", "vinyl", "utile", "smell", "adage", "worry", "tasty", "local", "trade", "finch", "ashen", "modal", "gaunt", "clove", "enact", "adorn", "roast", "speck", "sheik", "missy", "grunt", "snoop", "party", "touch", "mafia", "emcee", "array", "south", "vapid", "jelly", "skulk", "angst", "tubal", "lower", "crest", "sweat", "cyber", "adore", "tardy", "swami", "notch", "groom", "roach", "hitch", "young", "align", "ready", "frond", "strap", "puree", "realm", "venue", "swarm", "offer", "seven", "dryer", "diary", "dryly", "drank", "acrid", "heady", "theta", "junto", "pixie", "quoth", "bonus", "shalt", "penne", "amend", "datum", "build", "piano", "shelf", "lodge", "suing", "rearm", "coral", "ramen", "worth", "psalm", "infer", "overt", "mayor", "ovoid", "glide", "usage", "poise", "randy", "chuck", "prank", "fishy", "tooth", "ether", "drove", "idler", "swath", "stint", "while", "begat", "apply", "slang", "tarot", "radar", "credo", "aware", "canon", "shift", "timer", "bylaw", "serum", "three", "steak", "iliac", "shirk", "blunt", "puppy", "penal", "joist", "bunny", "shape", "beget", "wheel", "adept", "stunt", "stole", "topaz", "chore", "fluke", "afoot", "bloat", "bully", "dense", "caper", "sneer", "boxer", "jumbo", "lunge", "space", "avail", "short", "slurp", "loyal", "flirt", "pizza", "conch", "tempo", "droop", "plate", "bible", "plunk", "afoul", "savoy", "steep", "agile", "stake", "dwell", "knave", "beard", "arose", "motif", "smash", "broil", "glare", "shove", "baggy", "mammy", "swamp", "along", "rugby", "wager", "quack", "squat", "snaky", "debit", "mange", "skate", "ninth", "joust", "tramp", "spurn", "medal", "micro", "rebel", "flank", "learn", "nadir", "maple", "comfy", "remit", "gruff", "ester", "least", "mogul", "fetch", "cause", "oaken", "aglow", "meaty", "gaffe", "shyly", "racer", "prowl", "thief", "stern", "poesy", "rocky", "tweet", "waist", "spire", "grope", "havoc", "patsy", "truly", "forty", "deity", "uncle", "swish", "giver", "preen", "bevel", "lemur", "draft", "slope", "annoy", "lingo", "bleak", "ditty", "curly", "cedar", "dirge", "grown", "horde", "drool", "shuck", "crypt", "cumin", "stock", "gravy", "locus", "wider", "breed", "quite", "chafe", "cache", "blimp", "deign", "fiend", "logic", "cheap", "elide", "rigid", "false", "renal", "pence", "rowdy", "shoot", "blaze", "envoy", "posse", "brief", "never", "abort", "mouse", "mucky", "sulky", "fiery", "media", "trunk", "yeast", "clear", "skunk", "scalp", "bitty", "cider", "koala", "duvet", "segue", "creme", "super", "grill", "after", "owner", "ember", "reach", "nobly", "empty", "speed", "gipsy", "recur", "smock", "dread", "merge", "burst", "kappa", "amity", "shaky", "hover", "carol", "snort", "synod", "faint", "haunt", "flour", "chair", "detox", "shrew", "tense", "plied", "quark", "burly", "novel", "waxen", "stoic", "jerky", "blitz", "beefy", "lyric", "hussy", "towel", "quilt", "below", "bingo", "wispy", "brash", "scone", "toast", "easel", "saucy", "value", "spice", "honor", "route", "sharp", "bawdy", "radii", "skull", "phony", "issue", "lager", "swell", "urine", "gassy", "trial", "flora", "upper", "latch", "wight", "brick", "retry", "holly", "decal", "grass", "shack", "dogma", "mover", "defer", "sober", "optic", "crier", "vying", "nomad", "flute", "hippo", "shark", "drier", "obese", "bugle", "tawny", "chalk", "feast", "ruddy", "pedal", "scarf", "cruel", "bleat", "tidal", "slush", "semen", "windy", "dusty", "sally", "igloo", "nerdy", "jewel", "shone", "whale", "hymen", "abuse", "fugue", "elbow", "crumb", "pansy", "welsh", "syrup", "terse", "suave", "gamut", "swung", "drake", "freed", "afire", "shirt", "grout", "oddly", "tithe", "plaid", "dummy", "broom", "blind", "torch", "enemy", "again", "tying", "pesky", "alter", "gazer", "noble", "ethos", "bride", "extol", "decor", "hobby", "beast", "idiom", "utter", "these", "sixth", "alarm", "erase", "elegy", "spunk", "piper", "scaly", "scold", "hefty", "chick", "sooty", "canal", "whiny", "slash", "quake", "joint", "swept", "prude", "heavy", "wield", "femme", "lasso", "maize", "shale", "screw", "spree", "smoky", "whiff", "scent", "glade", "spent", "prism", "stoke", "riper", "orbit", "cocoa", "guilt", "humus", "shush", "table", "smirk", "wrong", "noisy", "alert", "shiny", "elate", "resin", "whole", "hunch", "pixel", "polar", "hotel", "sword", "cleat", "mango", "rumba", "puffy", "filly", "billy", "leash", "clout", "dance", "ovate", "facet", "chili", "paint", "liner", "curio", "salty", "audio", "snake", "fable", "cloak", "navel", "spurt", "pesto", "balmy", "flash", "unwed", "early", "churn", "weedy", "stump", "lease", "witty", "wimpy", "spoof", "saner", "blend", "salsa", "thick", "warty", "manic", "blare", "squib", "spoon", "probe", "crepe", "knack", "force", "debut", "order", "haste", "teeth", "agent", "widen", "icily", "slice", "ingot", "clash", "juror", "blood", "abode", "throw", "unity", "pivot", "slept", "troop", "spare", "sewer", "parse", "morph", "cacti", "tacky", "spool", "demon", "moody", "annex", "begin", "fuzzy", "patch", "water", "lumpy", "admin", "omega", "limit", "tabby", "macho", "aisle", "skiff", "basis", "plank", "verge", "botch", "crawl", "lousy", "slain", "cubic", "raise", "wrack", "guide", "foist", "cameo", "under", "actor", "revue", "fraud", "harpy", "scoop", "climb", "refer", "olden", "clerk", "debar", "tally", "ethic", "cairn", "tulle", "ghoul", "hilly", "crude", "apart", "scale", "older", "plain", "sperm", "briny", "abbot", "rerun", "quest", "crisp", "bound", "befit", "drawn", "suite", "itchy", "cheer", "bagel", "guess", "broad", "axiom", "chard", "caput", "leant", "harsh", "curse", "proud", "swing", "opine", "taste", "lupus", "gumbo", "miner", "green", "chasm", "lipid", "topic", "armor", "brush", "crane", "mural", "abled", "habit", "bossy", "maker", "dusky", "dizzy", "lithe", "brook", "jazzy", "fifty", "sense", "giant", "surly", "legal", "fatal", "flunk", "began", "prune", "small", "slant", "scoff", "torus", "ninny", "covey", "viper", "taken", "moral", "vogue", "owing", "token", "entry", "booth", "voter", "chide", "elfin", "ebony", "neigh", "minim", "melon", "kneed", "decoy", "voila", "ankle", "arrow", "mushy", "tribe", "cease", "eager", "birth", "graph", "odder", "terra", "weird", "tried", "clack", "color", "rough", "weigh", "uncut", "ladle", "strip", "craft", "minus", "dicey", "titan", "lucid", "vicar", "dress", "ditch", "gypsy", "pasta", "taffy", "flame", "swoop", "aloof", "sight", "broke", "teary", "chart", "sixty", "wordy", "sheer", "leper", "nosey", "bulge", "savor", "clamp", "funky", "foamy", "toxic", "brand", "plumb", "dingy", "butte", "drill", "tripe", "bicep", "tenor", "krill", "worse", "drama", "hyena", "think", "ratio", "cobra", "basil", "scrum", "bused", "phone", "court", "camel", "proof", "heard", "angel", "petal", "pouty", "throb", "maybe", "fetal", "sprig", "spine", "shout", "cadet", "macro", "dodgy", "satyr", "rarer", "binge", "trend", "nutty", "leapt", "amiss", "split", "myrrh", "width", "sonar", "tower", "baron", "fever", "waver", "spark", "belie", "sloop", "expel", "smote", "baler", "above", "north", "wafer", "scant", "frill", "awash", "snack", "scowl", "frail", "drift", "limbo", "fence", "motel", "ounce", "wreak", "revel", "talon", "prior", "knelt", "cello", "flake", "debug", "anode", "crime", "salve", "scout", "imbue", "pinky", "stave", "vague", "chock", "fight", "video", "stone", "teach", "cleft", "frost", "prawn", "booty", "twist", "apnea", "stiff", "plaza", "ledge", "tweak", "board", "grant", "medic", "bacon", "cable", "brawl", "slunk", "raspy", "forum", "drone", "women", "mucus", "boast", "toddy", "coven", "tumor", "truer", "wrath", "stall", "steam", "axial", "purer", "daily", "trail", "niche", "mealy", "juice", "nylon", "plump", "merry", "flail", "papal", "wheat", "berry", "cower", "erect", "brute", "leggy", "snipe", "sinew", "skier", "penny", "jumpy", "rally", "umbra", "scary", "modem", "gross", "avian", "greed", "satin", "tonic", "parka", "sniff", "livid", "stark", "trump", "giddy", "reuse", "taboo", "avoid", "quote", "devil", "liken", "gloss", "gayer", "beret", "noise", "gland", "dealt", "sling", "rumor", "opera", "thigh", "tonga", "flare", "wound", "white", "bulky", "etude", "horse", "circa", "paddy", "inbox", "fizzy", "grain", "exert", "surge", "gleam", "belle", "salvo", "crush", "fruit", "sappy", "taker", "tract", "ovine", "spiky", "frank", "reedy", "filth", "spasm", "heave", "mambo", "right", "clank", "trust", "lumen", "borne", "spook", "sauce", "amber", "lathe", "carat", "corer", "dirty", "slyly", "affix", "alloy", "taint", "sheep", "kinky", "wooly", "mauve", "flung", "yacht", "fried", "quail", "brunt", "grimy", "curvy", "cagey", "rinse", "deuce", "state", "grasp", "milky", "bison", "graft", "sandy", "baste", "flask", "hedge", "girly", "swash", "boney", "coupe", "endow", "abhor", "welch", "blade", "tight", "geese", "miser", "mirth", "cloud", "cabal", "leech", "close", "tenth", "pecan", "droit", "grail", "clone", "guise", "ralph", "tango", "biddy", "smith", "mower", "payee", "serif", "drape", "fifth", "spank", "glaze", "allot", "truck", "kayak", "virus", "testy", "tepee", "fully", "zonal", "metro", "curry", "grand", "banjo", "axion", "bezel", "occur", "chain", "nasal", "gooey", "filer", "brace", "allay", "pubic", "raven", "plead", "gnash", "flaky", "munch", "dully", "eking", "thing", "slink", "hurry", "theft", "shorn", "pygmy", "ranch", "wring", "lemon", "shore", "mamma", "froze", "newer", "style", "moose", "antic", "drown", "vegan", "chess", "guppy", "union", "lever", "lorry", "image", "cabby", "druid", "exact", "truth", "dopey", "spear", "cried", "chime", "crony", "stunk", "timid", "batch", "gauge", "rotor", "crack", "curve", "latte", "witch", "bunch", "repel", "anvil", "soapy", "meter", "broth", "madly", "dried", "scene", "known", "magma", "roost", "woman", "thong", "punch", "pasty", "downy", "knead", "whirl", "rapid", "clang", "anger", "drive", "goofy", "email", "music", "stuff", "bleep", "rider", "mecca", "folio", "setup", "verso", "quash", "fauna", "gummy", "happy", "newly", "fussy", "relic", "guava", "ratty", "fudge", "femur", "chirp", "forte", "alibi", "whine", "petty", "golly", "plait", "fleck", "felon", "gourd", "brown", "thrum", "ficus", "stash", "decry", "wiser", "junta", "visor", "daunt", "scree", "impel", "await", "press", "whose", "turbo", "stoop", "speak", "mangy", "eying", "inlet", "crone", "pulse", "mossy", "staid", "hence", "pinch", "teddy", "sully", "snore", "ripen", "snowy", "attic", "going", "leach", "mouth", "hound", "clump", "tonal", "bigot", "peril", "piece", "blame", "haute", "spied", "undid", "intro", "basal", "shine", "gecko", "rodeo", "guard", "steer", "loamy", "scamp", "scram", "manly", "hello", "vaunt", "organ", "feral", "knock", "extra", "condo", "adapt", "willy", "polka", "rayon", "skirt", "faith", "torso", "match", "mercy", "tepid", "sleek", "riser", "twixt", "peace", "flush", "catty", "login", "eject", "roger", "rival", "untie", "refit", "aorta", "adult", "judge", "rower", "artsy", "rural", "shave"]
#Copied from the JavaScript on wordle website https://www.powerlanguage.co.uk/wordle/
wordleList = list(wordleDict) #Used so the simulation is easy to track
#wordleList = ['shelf']
wordleDict = set(wordleDict)
guesses = []
class WordGuesser:

	def __init__(self):
		self.alphabet = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
		self.vowels = {'a', 'e', 'i', 'o', 'u', 'y'}
		self.consonants = self.alphabet - (self.vowels - {'y'}) #y is both vowel and consonant
		self.consonants = list(self.consonants)
		self.vowels = list(self.vowels)

		self.onsets = ['pl', 'bl', 'kl', 'cl', 'gl', 'pr', 'br', 'tr', 'dr', 'kr', 'cr', 'gr', 'tw', 'dw', 'fl', 'sl', 'th', 'sh', 'ch', 'vl', 'fr', 'th', 'wh', 'sw', 'thw', 'sp', 'st', 'sk', 'sm', 'sn', 'sph', 'sth', 'spl', 'scl', 'spr', 'str', 'scr', 'squ', 'qu', 'sm', 'sp', 'st', 'sk', 'sphr', 'ph', 'shr', 'fj', 'wr', 'rh', 'kn', 'sc', 'kh', 'thr', 'gh', 'gn', 'll', 'ps']
		self.onsets += self.consonants
		self.nucleus = ['ea', 'ie', 'ow', 'au', 'ah', 'aa', 'augh', 'al', 'oh', 'ay', 'ey', 'aye', 'eye', 'ai', 'ough', 'oo', 'ew', 'ue', 'igh', 'eigh', 'oa', 'ee', 'oi', 'oy', 'ou', 'oe', 'ur', 'ar', 'or', 'ear', 'eer', 'air', 'eir', 'ir', 'oar', 'our', 'ore', 'are','ure', 'er', 'eo', 'ei', 'eau', 'uar']
		self.nucleus += self.vowels
		self.coda = ['lp', 'lb', 'lt', 'ld', 'lch', 'ch', 'ge', 'rk', 'gue', 'lf', 'lve', 'lth', 'lse', 'lls', 'lsh', 've', 'ce', 'rs', 'rsh', 'lm', 'ln', 'mp', 'nt', 'nd', 'nch', 'nge', 'nk', 'mph', 'es', 'ez', 'mf', 'nf', 'mth', 'nth', 'nce', 'nze', 'ngth', 'ft', 'sp', 'st', 'sk', 'fth', 'pt' 'ct', 'pth', 'pse', 'ghth', 'tz', 'tze', 'dth', 'dze', 'xe', 'ck', 'cks', 'lpt', 'lps', 'lfth', 'ltz', 'lst', 'lct', 'lx', 'mth', 'pt', 'pse', 'tz', 'rst', 'ct', 'mpt', 'mpse', 'ntz', 'ndth', 'nct' 'nx', 'xth', 'xt', 'ch', 'sh', 'th', 'ng', 'gn', 'll', 'ff', 'ke', 'de', 'te', 'se', 'be', 'me', 'ne', 'wl', 'rd', 'tch', 'mb', 'lk', 'dst', 'bt', 'ph', 'wn', 'ngst', 'dd', 'sm', 'rrh', 'cht']
		self.coda += self.consonants
		self.suffix = ['s']

		self.green_letters = {}
		self.yellow_letters = {} #history of all index to yellow_letters
		self.yellow_letters_list = [] #updated list of current yellow_letters
		self.black_letters = set()
		self.operations = 0
		#need a separate list to contain a "set" of the yellow letters, that allows double letters. 
		#The yellow_letters dictionary contains a history of all guesses. 
		#But if position is wrong twice in a row it needs to be differentiated from two of the same letter

	def playGame(self, knownWord = None, dictionary = wordleDict, numLetters = 5):
		''' 
		knownWord is used by the simulator to test the code
		If there is no knownWord the user must provide feebdack
		It uses an initial guess hard coded in function
		The user must enter feedback in a string following the following convention:
		0 - letter not in word
		1 - letter in word, but in wrong position
		2 - letter in word at correct position
		example: word is storm
		Guess is pools
		User should enter 00201
		'''
		initial_guess = 'tears'
		print("guess is '", initial_guess, "'" )
		if not knownWord:
			feedback = input("feedback is: ")
		else:
			feedback = self.getFeedback(initial_guess, knownWord)
		self.updateInfo(initial_guess, feedback)
		while len(self.green_letters) != 5:
			guess = self.guessWord(dictionary, numLetters)
			if not guess:
				break
			print("guess is '", guess, "'" )
			if not knownWord:
				feedback = input("feedback is: ")
			else:
				feedback = self.getFeedback(guess, knownWord)	
			self.updateInfo(guess, feedback)
		guesses.append(self.operations)
		if len(self.green_letters) != 5:
			print ("I'm stumped")
			return False
		else:
			print ("Solved it!")
			return True

	def updateInfo(self, guess, result):
		self.updateKnownLetters(guess, result)
		self.updatePhonemes()

	def updateKnownLetters(self, guess, result):
		''' guess is original word
		    result is the evaluation
		    0 is not in word
		    1 is in word but different position
		    2 is in word at this position
		'''
		#Need to consider how guessing 2 A's and only having 1 a impacts this
		self.yellow_letters_list = [] #gets reset every time
		for i in range(len(result)):
			letter = guess[i]
			r = result[i]
			if r == '0':
				self.black_letters.add(letter)
			elif r == '1':
				if i in self.yellow_letters.keys():
					self.yellow_letters[i].append(letter)
				else:
					self.yellow_letters[i] = [letter]
				self.yellow_letters_list.append(letter)
			elif r == '2':
				#If upgraded from yellow, need to now delete it from yellow so not double counted
				if letter in self.yellow_letters_list:
					index = self.yellow_letters_list.index(letter)
					self.yellow_letters_list.pop(index)
				if letter in self.yellow_letters.values():
					for k in self.yellow_letters.keys():
						if self.yellow_letters[k] == letter:
							self.yellow_letters.pop(k)
							break
				self.green_letters[i] = letter
		
		# print('green', self.green_letters)
		# print('yello', self.yellow_letters)
		# print('black', self.black_letters)	
	
	def updatePhonemes(self) :
		black = (self.black_letters - set(self.green_letters.values())) - set(self.yellow_letters_list)
		#If a black letter is also a green or yellow, still need to add the syllables. Whole word will be checked later
		onsetsnew = []
		nucleusnew = []
		codanew = []
		suffixnew = []
		for cluster in self.onsets:
			toadd = True
			for letter in cluster:
				if letter in black:
					toadd = False
			if toadd:
				onsetsnew.append(cluster)
		for cluster in self.nucleus:
			toadd = True
			for letter in cluster:
				if letter in black:
					toadd = False
			if toadd:
				nucleusnew.append(cluster)
		for cluster in self.coda:
			toadd = True
			for letter in cluster:
				if letter in black:
					toadd = False
			if toadd:
				codanew.append(cluster)
		for cluster in self.suffix:
			toadd = True
			for letter in cluster:
				if letter in black:
					toadd = False
			if toadd:
				suffixnew.append(cluster)

		# print('old O', self.onsets)
		# print('old N', self.nucleus)
		# print('old C', self.coda)
		self.onsets = onsetsnew
		self.nucleus = nucleusnew
		self.coda = codanew
		self.suffix = suffixnew
		# print('new O', self.onsets)
		# print('new N', self.nucleus)
		# print('new C', self.coda)

	# def guessWord(self, dictionary, word_length = 5):
	# 	'''The guess must be a valid word in the dictinary'''
	# 	wordsToCheck = self.wordConstructor(word_length)
	# 	for word in wordsToCheck:
	# 		if word in dictionary and self.meetsRequirements(word):
	# 			return word
	# 	print ('No word found')
	# 	return None
	
	# def wordConstructor(self, word_length):
	# 	wordsinprogress = []
	# 	syllable_1 = self.syllableConstructor(1)
	# 	syllable_2 = self.syllableConstructor(2)
	# 	syllable_3 = self.syllableConstructor(3)
	# 	syllable_4 = self.syllableConstructor(4)
	# 	syllable_5 = self.syllableConstructor(5)
	# 	allsyllables = self.syllableConstructor(word_length)
	# 	wordsinprogress += syllable_5
	# 	wordsinprogress += self.addSyllables(syllable_1, syllable_4)
	# 	wordsinprogress += self.addSyllables(syllable_4, syllable_1)
	# 	wordsinprogress += self.addSyllables(syllable_2, syllable_3)
	# 	wordsinprogress += self.addSyllables(syllable_3, syllable_2)
	# 	wordsinprogress += self.addSyllables(syllable_1, self.addSyllables(syllable_2, syllable_2))
	# 	wordsinprogress += self.addSyllables(syllable_2, self.addSyllables(syllable_1, syllable_2))
	# 	wordsinprogress += self.addSyllables(syllable_2, self.addSyllables(syllable_2, syllable_1))
	# 	return wordsinprogress
	


	def guessWord(self, dictionary, limit=10):
		"""Return the word to guess
		Generate all the possible syllables but if valid guess immediately return
		Then construct word from syllables
		Done in one function for efficiency"""
		
		syllables = {}
		
		for i in range(1,limit+1):
			syllables[i] = []

		for n in self.nucleus:
			word = n
			self.operations +=1
			if len(word) <= limit:
				if len(word) == limit:
					if self.checkWord(word, dictionary):
						return word
					continue
				else:
					syllables[len(word)].append(word)
			else:
				continue
			for s in self.suffix:
				word = n + s
				self.operations +=1
				if len(word) <= limit:
					if len(word) == limit:
						if self.checkWord(word, dictionary):
							return word
						continue
					else:
						syllables[len(word)].append(word)
				else:
					continue

			for o in self.onsets:
				word = o + n
				self.operations +=1
				if len(word) <= limit:
					if len(word) == limit:
						if self.checkWord(word, dictionary):
							return word
						continue
					else:
						syllables[len(word)].append(word)
				else:
					continue
				for c in self.coda:
					word = o + n + c
					self.operations +=1
					if len(word) <= limit:
						if len(word) == limit:
							if self.checkWord(word, dictionary):
								return word
							continue
						else:
							syllables[len(word)].append(word)
					else:
						continue
					for s in self.suffix:
						word = o + n + c + s
						self.operations +=1
						if len(word) <= limit:
							if len(word) == limit:
								if self.checkWord(word, dictionary):
									return word
								continue
							else:
								syllables[len(word)].append(word)
						else:
							continue
			for c in self.coda:
				word = n + c
				self.operations +=1
				if len(word) <= limit:
					if len(word) == limit:
						if self.checkWord(word, dictionary):
							return word
						continue
					else:
						syllables[len(word)].append(word)
				else:
					continue
					for s in self.suffix:
						word = n + c + s
						self.operations +=1
						if len(word) <= limit:
							if len(word) == limit:
								if self.checkWord(word, dictionary):
									return word
								continue
							else:
								syllables[len(word)].append(word)
						else:
							continue
		
		word = self.addSyllablesTwo(syllables[1], syllables[4], dictionary)
		if word:
			return word
		word = self.addSyllablesTwo(syllables[2], syllables[3], dictionary)
		if word:
			return word
		word = self.addSyllablesThree(syllables[1], syllables[2], syllables[2], dictionary)
		if word:
			return word
		word = self.addSyllablesThree(syllables[1], syllables[3], syllables[1], dictionary)
		if word:
			return word
		print ('No word found')
		return None
	
	def addSyllablesTwo(self, a, b, dictionary):
		'''Return possible word from adding b to a'''
		for s1 in a:
			for s2 in b:
				if self.checkWord(s1+s2, dictionary):
					return s1+s2
				if self.checkWord(s2+s1, dictionary):
					return s2+s1
		return None

	def addSyllablesThree(self, a, b, c, dictionary):
		for s1 in a:
			for s2 in b:
				for s3 in c:
					if self.checkWord(s1+s2+s3, dictionary):
						return s1+s2+s3
					if self.checkWord(s1+s3+s2, dictionary):
						return s1+s3+s2
					if self.checkWord(s2+s1+s3, dictionary):
						return s2+s1+s3
					if self.checkWord(s2+s3+s1, dictionary):
						return s2+s3+s1
					if self.checkWord(s3+s1+s2, dictionary):
						return s3+s1+s2
					if self.checkWord(s3+s2+s1, dictionary):
						return s3+s2+s1

	def checkWord(self, word, dictionary):
		if word in dictionary and self.meetsRequirements(word):
				return word

	def meetsRequirements(self, word):
		''' return true if green, yellow, and black conditions are all met and false otherwise'''
		tempword = word

		for i in self.green_letters.keys():
			if tempword[i] == self.green_letters[i]:
				tempword = tempword[:i] + '0' + tempword[i+1:]
			else:
				return False
		
		for letter in self.yellow_letters_list:
			pos = tempword.find(letter)
			if pos == -1 or pos in self.yellow_letters.keys() and tempword[pos] in self.yellow_letters[pos]:
				return False
			else:
				tempword = tempword[:pos] + '0' + tempword[pos+1:]
		
		for i in range(len(word)):
			letter = tempword[i]
			if letter in self.black_letters:
				return False
		return True

	def getFeedback(self, guess, word):
		#Need to consider the double letter cases
		#Need to go through green first to account for this so not double counting
		result = ['0']*len(guess)

		for i in range(len(guess)):
			if guess[i] == word[i]:
				result[i] = '2'
				word = word[:i] + '0' + word[i+1:]
				guess = guess[:i] + '9' + guess[i+1:]

		for i in range(len(guess)):
			if guess[i] in word:
				result[i] = '1'
				wordindex = word.find(guess[i])
				word = word[:wordindex] + '0' + word[wordindex+1:]
				guess = guess[:i] + '9' + guess[i+1:]
		s = ""
		return s.join(result)

	# def guessWord(self, dictionary, limit=10):
	# 	'''The Lame Implementation (simpler and more efficient)'''
	# 	for word in dictionary:
	# 		if self.meetsRequirements(word):
	# 			return word


def simulator():
	''' Checks program on every possible word to guarentee functionality'''
	count = 0
	for word in wordleList:
		count+=1
		#guess word and if False print word and return False
		w = WordGuesser()
		res = w.playGame(word)
		if not res:
			print(word)
			print(count)
			print ('average words created', sum(guesses)/len(guesses))
			return False
		print(word)


simulator()
# w = WordGuesser()
# w.playGame()

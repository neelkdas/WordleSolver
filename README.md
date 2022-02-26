# WordleSolver
Inspired by the wordle game, I tried to create a solver in the spirit of an human English language solver
An overview of the game (https://www.powerlanguage.co.uk/wordle/):
 - There is an unknown five letter word you are attempting to guess.
 - A guess is only valid if it is an actual word in the dictionary
 - Once you have guessed a word, you are given feedback.
 	* A green letter means that letter is in the correct position.
 	* A yellow letter means that letter is in the word in a different position.
  	* A black letter means that letter is not in the word (or if you guessed two of the same letter, only one of them is in the word)
  	* To win, you must guess the word by the sixth try, but of course you want to guess it as few tries as possible.

The approach is to generate word guesses based on constructing syllables
Syllables are composed of three parts:
- A mandatory nucleus (vowel sound)
- An optional onset before the nucleus (consonant sound)
- An optional coda after the nucleus (consonant sound)
- The entire word may also contain a suffix (s at end of word)
For more information, refer to https://en.wikipedia.org/wiki/English_phonology

An alternative approach is to iterate through the dictionary and check if the word satisfies the requirements
Indeed, this approach is much simpler, and faster. However, each guess for that is O(D) where D is the size of the dictionary, and the syllable approach is way more interesting.
The syllable approach is more computationally intense in its current form. The wordle dictionary is ~5000 words. The entire english dictionary contains 171,000 words
A quick estimation of the syllabic complexity: 50 onsets * 50 nuclei, * 100 codas * 2 suffix is ~500,000 combinations for 1 syllable.
Then multiple syllables requires mixing syllables together, which is many more combinations.
This is a fun exercise though, and it is a more human approach to solving the problem and might be able to be better optimized at least in terms of solving it in a fewer number of guesses, especially given which phonemes are more common than others. 

Eventually I'd like to expand the program so that it can handle letter of any known length, and then even words of an unknown length.

TODO: Get statistics on the frequency of the phonemes to add a priority to the order of the lists
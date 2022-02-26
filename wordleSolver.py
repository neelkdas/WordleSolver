# Neel Das v2 02.21.2022
# The approach is to generate word guesses based on constructing syllables
# Syllables are composed of three parts:
#	A mandatory nucleus (vowel sound)
#	An optional onset before the nucleus (consonant sound)
#	An optional coda after the nucleus (consonant sound)
#	The entire word may also contain a suffix (s at end of word)
# For more information, refer to https://en.wikipedia.org/wiki/English_phonology
#
# An alternative approach is to iterate through the dictionary and check if the word satisfies the requirements
# Indeed, this approach is much simpler, and faster. However, each guess for that is O(D) and the syllable approach is way more interesting
# The syllable approach is more computationally intense as well. The wordle dictionary is ~5000 words. The english dictionary contains 171,000 words
# 50 onsets * 50 nuclei, * 100 codas * 2 suffix ~500,000 combinations for 1 syllable.
# This is a fun exercise though, and it is a more human approach to solving the problem and might be able to be better optimized


import wordleData
complexity = []
guesses = []
class WordleSolver:

	def __init__(self):
		self.onsets = wordleData.onsets
		self.nucleus = wordleData.nucleus
		self.coda = wordleData.coda
		self.suffix = wordleData.suffix
		self.green_letters = {}
		self.yellow_letters_index_history = {} #history of all index to yellow letters
		self.yellow_letters_current_list = [] 
		self.black_letters = set()
		self.operations = 0 #For estimating complexity of program
		#need a separate list to contain a "set" of the yellow letters, that allows double letters. 
		#The yellow_letters_index_history dictionary contains a history of all guesses. 
		#But if position is wrong twice in a row it needs to be differentiated from two of the same letter

	def play_game(self, known_word = None, dictionary = wordleData.WORDLE_DICT, numLetters = 5):
		''' 
		known_word is used by the simulator to test the code
		If there is no known_word the user must provide feebdack
		It uses an initial guess hard coded in function
		The user must enter feedback in a string following the following convention:
		0 - letter not in word
		1 - letter in word, but in wrong position
		2 - letter in word at correct position
		example: word is storm	
		Guess is pools
		User should enter 00201
		'''
		initial_guess = 'tears' #Usually my initial guess. Could be further optimized.
		print("guess is '", initial_guess, "'" )
		if not known_word:
			print("Provide feedback in a 5 character string. 0 for letter is not in word. 1 for letter in word in wrong position. 2 for letter in word at correct position.")
			feedback = input("feedback is: ")
		else:
			feedback = self.get_feedback(initial_guess, known_word)
		self.update_info(initial_guess, feedback)
		guesses.append(1)
		while len(self.green_letters) != 5:
			guess = self.guess_word(dictionary, numLetters)
			guesses[-1] += 1
			if not guess:
				break
			print("guess is '", guess, "'" )
			if not known_word:
				feedback = input("feedback is: ")
			else:
				feedback = self.get_feedback(guess, known_word)	
			self.update_info(guess, feedback)
		complexity.append(self.operations)
		if len(self.green_letters) != 5:
			print("I'm stumped")
			return False
		else:
			print("Solved it!")
			return True

	def update_info(self, guess, result):
		self.update_known_letters(guess, result)
		self.update_phonemes()

	def update_known_letters(self, guess, result):
		''' guess is original word
		    result is the evaluation
		    0 is not in word
		    1 is in word but different position
		    2 is in word at this position
		'''
		#Need to consider how guessing 2 A's and only having 1 a impacts this
		self.yellow_letters_current_list = [] #gets reset every time
		for i in range(len(result)):
			letter = guess[i]
			r = result[i]
			if r == '0':
				self.black_letters.add(letter)
			elif r == '1':
				if i in self.yellow_letters_index_history.keys():
					self.yellow_letters_index_history[i].append(letter)
				else:
					self.yellow_letters_index_history[i] = [letter]
				self.yellow_letters_current_list.append(letter)
			elif r == '2':
				#If upgraded from yellow, need to now delete it from yellow so not double counted
				if letter in self.yellow_letters_current_list:
					index = self.yellow_letters_current_list.index(letter)
					self.yellow_letters_current_list.pop(index)
				if letter in self.yellow_letters_index_history.values():
					for k in self.yellow_letters_index_history.keys():
						if self.yellow_letters_index_history[k] == letter:
							self.yellow_letters_index_history.pop(k)
							break
				self.green_letters[i] = letter	
	
	def update_phonemes(self):
		def update_list(list_to_update, list_to_update_from, list_to_check):
			for cluster in list_to_update_from:
				to_add = True
				for letter in cluster:
					if letter in list_to_check:
						to_add = False
						break
				if to_add:
					list_to_update.append(cluster)

		black = (self.black_letters - set(self.green_letters.values())) - set(self.yellow_letters_current_list)
		#If a black letter is also a green or yellow, still need to add the syllables. Whole word will be checked later
		onsets_new = []
		nucleus_new = []
		coda_new = []
		suffix_new = []
		
		update_list(onsets_new, self.onsets, black)
		update_list(nucleus_new, self.nucleus, black)
		update_list(coda_new, self.coda, black)
		update_list(suffix_new, self.suffix, black)

		self.onsets = onsets_new
		self.nucleus = nucleus_new
		self.coda = coda_new
		self.suffix = suffix_new
	
	def guess_word_lame(self, dictionary, limit=10):
		'''The Lame Implementation (simpler and more efficient)'''
		for word in dictionary:
			if self.meets_requirements(word):
				return word

	def guess_word(self, dictionary, limit=10):
		"""Return the word to guess
		Generate all the possible syllables but if valid guess immediately return
		Then construct word from syllables
		Terribly ugly, would appreciate a recommendation on how to simplerlify this"""
		
		syllables = {}
		
		for i in range(1,limit+1):
			syllables[i] = [] # number of letters -> list with syllables of that many letters

		for n in self.nucleus:
			word = n
			self.operations +=1 # for estimating complexity of program
			if len(word) <= limit:
				if len(word) == limit:
					if self.check_word(word, dictionary):
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
						if self.check_word(word, dictionary):
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
						if self.check_word(word, dictionary):
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
							if self.check_word(word, dictionary):
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
								if self.check_word(word, dictionary):
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
						if self.check_word(word, dictionary):
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
								if self.check_word(word, dictionary):
									return word
								continue
							else:
								syllables[len(word)].append(word)
						else:
							continue
		
		word = self.add_syllables_two(syllables[1], syllables[4], dictionary)
		if word:
			return word
		word = self.add_syllables_two(syllables[2], syllables[3], dictionary)
		if word:
			return word
		word = self.add_syllables_three(syllables[1], syllables[2], syllables[2], dictionary)
		if word:
			return word
		word = self.add_syllables_three(syllables[1], syllables[3], syllables[1], dictionary)
		if word:
			return word
		print ('No word found')
		return None
	
	def add_syllables_two(self, a, b, dictionary):
		'''Return possible word from adding b to a'''
		for s1 in a:
			for s2 in b:
				if self.check_word(s1+s2, dictionary):
					return s1+s2
				if self.check_word(s2+s1, dictionary):
					return s2+s1
		return None

	def add_syllables_three(self, a, b, c, dictionary):
		# TODO: Combine add_syllables functions
		for s1 in a:
			for s2 in b:
				for s3 in c:
					if self.check_word(s1+s2+s3, dictionary):
						return s1+s2+s3
					if self.check_word(s1+s3+s2, dictionary):
						return s1+s3+s2
					if self.check_word(s2+s1+s3, dictionary):
						return s2+s1+s3
					if self.check_word(s2+s3+s1, dictionary):
						return s2+s3+s1
					if self.check_word(s3+s1+s2, dictionary):
						return s3+s1+s2
					if self.check_word(s3+s2+s1, dictionary):
						return s3+s2+s1

	def check_word(self, word, dictionary):
		if word in dictionary and self.meets_requirements(word):
			return word

	def meets_requirements(self, word):
		''' return true if green, yellow, and black conditions are all met and false otherwise'''
		tempword = word

		for i in self.green_letters.keys():
			if tempword[i] == self.green_letters[i]:
				tempword = tempword[:i] + '0' + tempword[i+1:]
			else:
				return False
		
		for letter in self.yellow_letters_current_list:
			pos = tempword.find(letter)
			if (pos == -1) or (pos in self.yellow_letters_index_history.keys() and tempword[pos] in self.yellow_letters_index_history[pos]):
				return False
			else:
				tempword = tempword[:pos] + '0' + tempword[pos+1:]
		
		for i in range(len(word)):
			letter = tempword[i]
			if letter in self.black_letters:
				return False
		return True

	def get_feedback(self, guess, word):
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





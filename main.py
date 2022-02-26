import wordleSolver
import wordleData
def simulator():
	''' Checks program on every possible word to guarantee functionality'''
	count = 0
	for word in wordleData.WORDLE_LIST:
		count+=1
		#guess word and if False print word and return False
		w = wordleSolver.WordleSolver()
		res = w.play_game(word)
		if not res:
			print(word)
			print(count)
			print ('average operations per wordle', sum(wordleSolver.complexity)/len(wordleSolver.complexity))
			print ('average guesses per wordle', sum(wordleSolver.guesses)/len(wordleSolver.guesses))
			return False
		print(word)
	print ('average operations per wordle', sum(wordleSolver.complexity)/len(wordleSolver.complexity)) #60,000
	print ('average guesses per wordle', sum(wordleSolver.guesses)/len(wordleSolver.guesses)) #3.8

if __name__ == "__main__":
	# simulator() # Takes about 2.5 minutes to run on my computer
	w = wordleSolver.WordleSolver()
	w.play_game() #free play
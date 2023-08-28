
Recurrent Neural Network 
https://www.youtube.com/watch?v=Y2wfIKQyd1I&list=PLeo1K3hjS3us_ELKYSj_Fth2tIEkdKXvV&index=106&ab_channel=codebasics

Artificial NN/CNN/RNN (https://chat.openai.com/ diff btwn ANN, CNN, RNN)
1. neural networks: regression, simple classification but does not consider spatial relationships or sequential data
2. convolutional neural networks: mostly for image and spatial data, object detection, image detection
	- exploits spatial hierarchies and local patterns in the data
3. recurrent neural network: for natural language processing (NLP)- where sequence is matters;
 e.g.
	a) autocompleting sentences/language modelling/speech recognition
 	b) language translation
	c) name entry recognition (name, city capitalization)

Problem of using ANN for sequence cases (i.e. why RNN is better)
a) variable size of input and output (not fixed: one word in one language can have multiple
words in another as its translation)
b) Too much computation
c) There are no shared parameters
d) can capture temporal dependencies but not very good for long sequences coz of a short memory
	- the remedies for the short term memory
	-- LSTM (long and short term memory)
	-- GRU (gated recurrent units) which is a lighter version of LSTM

Computer Vision:

https://www.youtube.com/watch?v=aDpnaxPAmtU&list=PLeo1K3hjS3us_ELKYSj_Fth2tIEkdKXvV&index=95

1. tensorflow - free and open-source software library for machine learning and artificial intelligence 

	      - from Google

2. pytorch - facebook

3. keras - rapper of tensorflow, CNTK & theano. It is not full fledged on its own
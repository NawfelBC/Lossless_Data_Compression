# Lossless Data Compression

## Summary

In this class project, we studied the concept of lossless data compression. It allows to reduce considerably the size of data, which is very useful to transfer files through a network, because the smaller the file size, the faster the transfer. In order to study these concepts, we have therefore carried out research to understand and implement in the Scala language the different compression methods using algorithmic concepts.

## Different methods

[RLE :](https://github.com/NawfelBC/Lossless_Data_Compression/blob/main/RLE.scala)
Run-length encoding (RLE) is a form of lossless data compression in which runs of data (sequences in which the same data value occurs in many consecutive data elements) are stored as a single data value and count, rather than as the original run.

[Huffman :](https://github.com/NawfelBC/Lossless_Data_Compression/blob/main/statistic/Huffman.scala)
The idea of Huffman coding is to assign variable-length codes to input characters, lengths of the assigned codes are based on the frequencies of corresponding characters. The most frequent character gets the smallest code and the least frequent character gets the largest code.

[Shannon-Fano :](https://github.com/NawfelBC/Lossless_Data_Compression/blob/main/statistic/ShannonFano.scala)
Shannon–Fano coding is a technique for constructing a prefix code based on a set of symbols and their probabilities. It is suboptimal in the sense that it does not achieve the lowest possible expected codeword length like Huffman coding.

[LZ78 :](https://github.com/NawfelBC/Lossless_Data_Compression/blob/main/lz/LZ78.scala)
LZ78-based schemes work by entering phrases into a dictionary and then, when a repeat occurrence of that particular phrase is found, outputting the dictionary index instead of the phrase.

[LZW :](https://github.com/NawfelBC/Lossless_Data_Compression/blob/main/lz/LZW.scala)
LZW compression works by reading a sequence of symbols, grouping the symbols into strings, and converting the strings into codes. Because the codes take up less space than the strings they replace, we get compression.

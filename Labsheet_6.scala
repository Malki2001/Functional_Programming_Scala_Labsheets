object Caesar_cipher {

  def caesarEncrypt(plaintext: String, shift: Int): String = {

    val encryptedChars = plaintext.map { char =>   //map function to iterate through each character in the plaintext string.
      if (char.isLetter) {
        val shiftOffset = if (char.isUpper) 'A' else 'a'
        val encryptedChar = ((char - shiftOffset + shift) % 26 + shiftOffset).toChar  //ascii_value.toChar
        //modular arithmetic to handle wrapping around the alphabet.
        encryptedChar

      } else
      {
        char
      }
    }

    encryptedChars.mkString
  }
                    //encryted text
  def caesarDecrypt(ciphertext: String, shift: Int): String = {

    caesarEncrypt(ciphertext, -shift)  //uses the caesarEncrypt() by calling it with a negative value of shift to reverse the encryption.
  }

  def cipher(text: String, shift: Int, operation: String): String = {
    operation match {
      case "encrypt" => caesarEncrypt(text, shift)
      case "decrypt" => caesarDecrypt(text, shift)
      case _ => throw new IllegalArgumentException("Invalid operation. Use 'encrypt' or 'decrypt'.")
    }
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Julius Caesar"
    val shiftAmount = 2

    // Encrypt the plaintext
    val encryptedText = cipher(plaintext, shiftAmount, "encrypt")
    println("Encrypted:"+encryptedText)

    // Decrypt the ciphertext
    val decryptedText =cipher(encryptedText, shiftAmount, "decrypt")
    println("Decrypted:"+decryptedText)
  }


}






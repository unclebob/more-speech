package ecdhJava;

import java.math.BigInteger;
import static javax.xml.bind.DatatypeConverter.parseHexBinary;

public class ecdh {
  public static void main(String[] args) throws Exception {
    String mySecKeyString = "0000000000000000000000000000000000000000000000000000000000002222";
    String hisPubKeyString = "2ef93f01cd2493e04235a6b87b10d3c4a74e2a7eb7c3caf168268f6af73314b5";
    byte[] mySecKeyBytes = parseHexBinary(mySecKeyString);
    byte[] hisPubKeyBytes = parseHexBinary(hisPubKeyString);
    BigInteger sharedKey = SECP256K1.calculateKeyAgreement(new BigInteger(mySecKeyBytes), new BigInteger(hisPubKeyBytes));
    System.out.println("secretKey = " + sharedKey.toString(16));
    String msg = SECP256K1.encrypt(sharedKey, "hi");
    System.out.println("msg = " + msg);
    String decodedMessage = SECP256K1.decrypt(sharedKey, msg);
    System.out.println("decodedMessage = " + decodedMessage);
  }
}

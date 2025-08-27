/**
 * HelloWorld.java - Simple Java hello world program
 * Created by Waldo 2025-08-27
 * 
 * This program verifies basic Java functionality in the SeqWeb environment.
 */

public class HelloWorld {
    
    public static void main(String[] args) {
        System.out.println("Hello, World!");
        System.out.println("Java is working in the SeqWeb environment!");
        
        // Verify some basic Java functionality
        System.out.println("Java version: " + System.getProperty("java.version"));
        System.out.println("Java vendor: " + System.getProperty("java.vendor"));
        
        // Test basic string operations
        String test = "Hello SeqWeb";
        System.out.println("✓ String operations working: " + test.toUpperCase());
        
        // Test basic math
        int result = 2 + 2;
        System.out.println("✓ Basic math working: 2 + 2 = " + result);
        
        System.out.println("Java environment verification complete!");
    }
}

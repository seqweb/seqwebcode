import java.util.*;
import java.io.*;
import java.util.function.Function;

/**
 * Test suite for Wrapper.java functionality.
 * 
 * This class tests the Java equivalent of the Python wrapper utilities,
 * including typed-value registry, conversion functions, and inbox/outbox handling.
 */
public class TestWrapper {
    
    private static int testsPassed = 0;
    private static int testsFailed = 0;
    
    public static void main(String[] args) {
        System.out.println("Testing Java Wrapper functionality...");
        System.out.println("=====================================");
        
        // Test typed-value registry
        testTypedValueRegistry();
        
        // Test conversion functions
        testConversionFunctions();
        
        // Test inbox functions
        testInboxFunctions();
        
        // Test outbox functions
        testOutboxFunctions();
        
        // Print summary
        System.out.println("\n=====================================");
        System.out.println("Test Results:");
        System.out.println("  Passed: " + testsPassed);
        System.out.println("  Failed: " + testsFailed);
        System.out.println("  Total:  " + (testsPassed + testsFailed));
        
        if (testsFailed == 0) {
            System.out.println("\n✅ All tests passed!");
        } else {
            System.out.println("\n❌ Some tests failed!");
            System.exit(1);
        }
    }
    
    private static void testTypedValueRegistry() {
        System.out.println("\n--- Testing Typed-Value Registry ---");
        
        // Test getting external type ID (should return null since no types are registered yet)
        String externalId = Wrapper.getExternalTypeId(String.class);
        assertEqual(externalId, null, "getExternalTypeId for unregistered type");
        
        // Test getting internal type
        Class<?> internalType = Wrapper.getInternalType("https://example.org/unknown");
        assertEqual(internalType, null, "getInternalType for unknown external ID");
        
        // Test getting conversion functions
        Function<Object, String> toJsonFunc = Wrapper.getToJsonFunction("https://example.org/unknown");
        assertEqual(toJsonFunc, null, "getToJsonFunction for unknown external ID");
        
        Function<String, Object> fromJsonFunc = Wrapper.getFromJsonFunction("https://example.org/unknown");
        assertEqual(fromJsonFunc, null, "getFromJsonFunction for unknown external ID");
        
        // Test checking if type is registered
        boolean isRegistered = Wrapper.isRegisteredType("test string");
        assertEqual(isRegistered, false, "isRegisteredType for unregistered type");
    }
    
    private static void testConversionFunctions() {
        System.out.println("\n--- Testing Conversion Functions ---");
        
        // Test converting simple types
        Map<String, Object> simpleData = new HashMap<>();
        simpleData.put("id", "test123");
        simpleData.put("value", 42);
        simpleData.put("flag", true);
        
        Object converted = Wrapper.convertToJson(simpleData);
        assertTrue(converted instanceof Map, "convertToJson returns Map for simple data");
        
        @SuppressWarnings("unchecked")
        Map<String, Object> convertedMap = (Map<String, Object>) converted;
        assertEqual(convertedMap.get("id"), "test123", "convertToJson preserves string values");
        assertEqual(convertedMap.get("value"), 42, "convertToJson preserves integer values");
        assertEqual(convertedMap.get("flag"), true, "convertToJson preserves boolean values");
        
        // Test round-trip conversion
        Object roundTrip = Wrapper.convertFromJson(converted);
        assertTrue(roundTrip instanceof Map, "convertFromJson returns Map for round-trip");
        
        @SuppressWarnings("unchecked")
        Map<String, Object> roundTripMap = (Map<String, Object>) roundTrip;
        assertEqual(roundTripMap.get("id"), "test123", "round-trip preserves string values");
        assertEqual(roundTripMap.get("value"), 42, "round-trip preserves integer values");
        assertEqual(roundTripMap.get("flag"), true, "round-trip preserves boolean values");
        
        // Test converting nested structures
        Map<String, Object> nestedData = new HashMap<>();
        nestedData.put("simple", "value");
        nestedData.put("nested", simpleData);
        
        Object nestedConverted = Wrapper.convertToJson(nestedData);
        assertTrue(nestedConverted instanceof Map, "convertToJson handles nested structures");
        
        // Test converting lists
        List<Object> listData = Arrays.asList("item1", "item2", 42);
        Object listConverted = Wrapper.convertToJson(listData);
        assertTrue(listConverted instanceof List, "convertToJson handles lists");
    }
    
    private static void testInboxFunctions() {
        System.out.println("\n--- Testing Inbox Functions ---");
        
        // Test augmentInboxWithArgs with empty definitions
        Map<String, Object> inbox = new HashMap<>();
        inbox.put("id", "test");
        inbox.put("value", 42);
        
        List<Wrapper.ArgumentDefinition> emptyDefs = new ArrayList<>();
        Map<String, Object> result = Wrapper.augmentInboxWithArgs(inbox, emptyDefs);
        assertEqual(result, inbox, "augmentInboxWithArgs with empty definitions");
        
        // Test augmentInboxWithArgs with CLI arguments (simulated)
        List<Wrapper.ArgumentDefinition> argDefs = Arrays.asList(
            new Wrapper.ArgumentDefinition("id", String.class, "The ID", true),
            new Wrapper.ArgumentDefinition("noisy", Boolean.class, "Verbose output", false)
        );
        
        // Note: This test is limited because we can't easily mock System.getProperty
        // In a real test environment, you'd want to use a proper mocking framework
        Map<String, Object> cliResult = Wrapper.augmentInboxWithArgs(inbox, argDefs);
        assertTrue(cliResult instanceof Map, "augmentInboxWithArgs returns Map");
        
        // Test getInbox (also limited by System.getProperty mocking)
        Map<String, Object> getInboxResult = Wrapper.getInbox(argDefs);
        assertTrue(getInboxResult instanceof Map, "getInbox returns Map");
    }
    
    private static void testOutboxFunctions() {
        System.out.println("\n--- Testing Outbox Functions ---");
        
        // Test toJson with simple data
        Map<String, Object> simpleData = new HashMap<>();
        simpleData.put("id", "test123");
        simpleData.put("value", 42);
        simpleData.put("flag", true);
        
        String json = Wrapper.toJson(simpleData);
        assertTrue(json.contains("\"id\":\"test123\""), "toJson includes id field");
        assertTrue(json.contains("\"value\":42"), "toJson includes value field");
        assertTrue(json.contains("\"flag\":true"), "toJson includes flag field");
        
        // Test toJson with null values
        Map<String, Object> nullData = new HashMap<>();
        nullData.put("nullValue", null);
        nullData.put("stringValue", "test");
        
        String nullJson = Wrapper.toJson(nullData);
        assertTrue(nullJson.contains("\"nullValue\":null"), "toJson handles null values");
        assertTrue(nullJson.contains("\"stringValue\":\"test\""), "toJson handles string values");
        
        // Test toJson with empty map
        String emptyJson = Wrapper.toJson(new HashMap<>());
        assertEqual(emptyJson, "{}", "toJson with empty map");
        
        // Test toJson with null map
        String nullMapJson = Wrapper.toJson(null);
        assertEqual(nullMapJson, "{}", "toJson with null map");
    }
    
    // Helper methods for testing
    private static void assertEqual(Object actual, Object expected, String testName) {
        if (Objects.equals(actual, expected)) {
            System.out.println("  ✅ " + testName);
            testsPassed++;
        } else {
            System.out.println("  ❌ " + testName + " - Expected: " + expected + ", Got: " + actual);
            testsFailed++;
        }
    }
    
    private static void assertTrue(boolean condition, String testName) {
        if (condition) {
            System.out.println("  ✅ " + testName);
            testsPassed++;
        } else {
            System.out.println("  ❌ " + testName + " - Condition was false");
            testsFailed++;
        }
    }
}
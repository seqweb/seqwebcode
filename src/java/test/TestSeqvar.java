import lib.seqvar.SeqVar;
import lib.seqvar.SeqvarToml;
import java.util.Map;

/**
 * Test program for Java SeqVar implementation
 */
public class TestSeqvar {
    
    public static void main(String[] args) {
        try {
            System.out.println("Testing Java SeqVar implementation...");
            
            // Test basic get/set operations
            System.out.println("\n1. Testing basic get/set operations:");
            SeqVar.set("test.key", "test value");
            String value = SeqVar.get("test.key");
            System.out.println("   Set 'test.key' = 'test value'");
            System.out.println("   Get 'test.key' = '" + value + "'");
            
            // Test source auto-detection
            System.out.println("\n2. Testing source auto-detection:");
            SeqVar.set("auto.source.test", "auto-detected source");
            String value2 = SeqVar.get("auto.source.test");
            System.out.println("   Set 'auto.source.test' = 'auto-detected source'");
            System.out.println("   Get 'auto.source.test' = '" + value2 + "'");
            
            // Test getDict functionality
            System.out.println("\n3. Testing getDict functionality:");
            Map<String, String> allVars = SeqVar.getDict();
            System.out.println("   All seqvars:");
            for (Map.Entry<String, String> entry : allVars.entrySet()) {
                System.out.println("     " + entry.getKey() + " = " + entry.getValue());
            }
            
            // Test pattern filtering
            System.out.println("\n4. Testing pattern filtering:");
            Map<String, String> testVars = SeqVar.getDict("test.*");
            System.out.println("   Test vars (pattern 'test.*'):");
            for (Map.Entry<String, String> entry : testVars.entrySet()) {
                System.out.println("     " + entry.getKey() + " = " + entry.getValue());
            }
            
            // Test dump functionality
            System.out.println("\n5. Testing dump functionality:");
            var dumpResults = SeqVar.dump();
            System.out.println("   Dump results:");
            for (Object[] row : dumpResults) {
                String key = (String) row[0];
                String val = (String) row[1];
                String src = (String) row[2];
                Long ts = (Long) row[3];
                System.out.println("     " + key + " = " + val + " (src: " + src + ", ts: " + ts + ")");
            }
            
            System.out.println("\n✅ All Java SeqVar tests completed successfully!");
            
        } catch (Exception e) {
            System.err.println("❌ Error during Java SeqVar testing: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}

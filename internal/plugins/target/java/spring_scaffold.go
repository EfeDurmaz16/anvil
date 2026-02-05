package java

const pomXML = `<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>3.2.0</version>
    </parent>
    <groupId>com.anvil</groupId>
    <artifactId>generated</artifactId>
    <version>1.0.0</version>
    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter</artifactId>
        </dependency>
        <dependency>
            <groupId>com.fasterxml.jackson.core</groupId>
            <artifactId>jackson-databind</artifactId>
        </dependency>
    </dependencies>
</project>`

const applicationJava = `package com.anvil.generated;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
`

const anvilManifestJSON = `{
  "version": "1",
  "language": "java",
  "compile": [
    { "cmd": "mvn", "args": ["compile"] }
  ],
  "run_fixture": { "cmd": "java", "args": ["-cp", "target/classes", "com.anvil.generated.AnvilRunner"] }
}
`

const anvilRunnerJava = `package com.anvil.generated;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;

public class AnvilRunner {
    private static final ObjectMapper mapper = new ObjectMapper();

    public static void main(String[] args) {
        try {
            // Read fixture from stdin
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                sb.append(line);
            }

            String input = sb.toString();
            JsonNode fixture = null;
            try {
                fixture = mapper.readTree(input);
            } catch (Exception e) {
                // ignore
            }

            // Build response
            Map<String, Object> output = new HashMap<>();
            output.put("status", 501);
            Map<String, String> headers = new HashMap<>();
            headers.put("content-type", "application/json");
            output.put("headers", headers);

            Map<String, Object> body = new HashMap<>();
            body.put("error", "not implemented");
            body.put("fixture", fixture != null && fixture.has("name") ? fixture.get("name").asText() : null);
            output.put("body", body);

            // Write response to stdout
            System.out.print(mapper.writeValueAsString(output));
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }
}
`

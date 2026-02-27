package harness

import (
	"testing"
)

func TestValidateCommand_AllowedCommands(t *testing.T) {
	allowed := []string{
		"node", "npx", "tsc",
		"python", "python3",
		"go",
		"javac", "mvn", "gradle",
		"npm", "pip",
	}
	for _, cmd := range allowed {
		t.Run(cmd, func(t *testing.T) {
			c := Command{Cmd: cmd}
			if err := validateCommand(c); err != nil {
				t.Errorf("expected command %q to be allowed, got error: %v", cmd, err)
			}
		})
	}
}

func TestValidateCommand_BlockedCommands(t *testing.T) {
	blocked := []string{"rm", "curl", "wget", "sh", "bash"}
	for _, cmd := range blocked {
		t.Run(cmd, func(t *testing.T) {
			c := Command{Cmd: cmd}
			if err := validateCommand(c); err == nil {
				t.Errorf("expected command %q to be blocked, but got no error", cmd)
			}
		})
	}
}

func TestValidateCommand_AbsolutePathRejected(t *testing.T) {
	c := Command{Cmd: "/usr/bin/node"}
	if err := validateCommand(c); err == nil {
		t.Error("expected absolute command path to be rejected, but got no error")
	}
}

func TestValidateCommand_DotDotInDirRejected(t *testing.T) {
	cases := []string{
		"../etc",
		"foo/../../etc",
		"subdir/../..",
	}
	for _, dir := range cases {
		t.Run(dir, func(t *testing.T) {
			c := Command{Cmd: "node", Dir: dir}
			if err := validateCommand(c); err == nil {
				t.Errorf("expected Dir %q with '..' to be rejected, but got no error", dir)
			}
		})
	}
}

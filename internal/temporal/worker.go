package temporal

import (
	"fmt"

	"go.temporal.io/sdk/client"
	"go.temporal.io/sdk/worker"
)

// StartWorker creates and starts a Temporal worker.
func StartWorker(c client.Client, taskQueue string) (worker.Worker, error) {
	w := worker.New(c, taskQueue, worker.Options{})

	w.RegisterWorkflow(ModernizationWorkflow)
	w.RegisterActivity(CartographerActivity)
	w.RegisterActivity(SpecularActivity)
	w.RegisterActivity(ArchitectActivity)
	w.RegisterActivity(JudgeActivity)
	w.RegisterActivity(HarnessActivity)

	if err := w.Start(); err != nil {
		return nil, fmt.Errorf("starting worker: %w", err)
	}
	return w, nil
}
